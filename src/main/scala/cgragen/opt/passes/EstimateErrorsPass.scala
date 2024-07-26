package cgragen.opt.passes

import cgragen.chiselTestAnnos

import cgragen.cgra.{CGRA, AbstractModule, AbstractOperation}, CGRA.supportedApprxOps
import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse.DFG
import cgragen.dfgparse.Opcode.{OpAdd, OpMul, OpSub}

import cgragen.opt.Pass

import approx.addition.AdaptiveOFLOCA
import approx.multiplication.AdaptiveRadix2Multiplier

import chiselverify.approximation.{ErrorReporter, track}
import chiselverify.approximation.Metrics.{MRED, SDRED}

import chisel3._
import chisel3.util.log2Up
import chiseltest._

import java.io.{ByteArrayOutputStream, File, PrintWriter}
import java.nio.file.Files

import scala.collection.mutable

import scala.util.{Left, Random, Right}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.run

import scala.xml.PrettyPrinter
import scala.xml.XML.loadFile

/** Estimate error characteristics of supported approximate arithmetic operations
 * 
 * @note This pass can operate independently of the DFG.
 */
private[cgragen] case object EstimateErrorsPass
  extends Pass(Seq.empty[Pass], Seq.empty[Pass]) {
  /** Path to error characteristic information
   * 
   * @note The file is organized into the following structure:
   *       <ErrorInfo>
   *         <operation opcode="...">
   *           <conf info="..." mred="..." sdred="..."/> ...
   *         </operation> ...
   *       </ErrorInfo>
   */
  private[cgragen] final val storagePath = "./src/main/resources/errors.xml"

  // Common type for processing error characteristic information (opcode string 
  // to map of (string of underscore-separated arguments to (MRED, SDRED)))
  private type ErrorInfo = Map[String, Map[String, (Double, Double)]]
  private object ErrorInfo {
    def empty: ErrorInfo = Map.empty[String, Map[String, (Double, Double)]]
  }

  /** Read the existing error characteristic information from the storage
   * @return the existing error characteristic information in a map
   */
  private def _readChars(): ErrorInfo = {
    if (!(new File(storagePath)).exists()) {
      // If the file does not yet exist, return an empty map
      ErrorInfo.empty
    } else {
      // Otherwise, read the file and extract its information
      val xml = loadFile(storagePath)

      // Check that the XML has the error characteristics tag
      (xml \\ "ErrorInfo").length match {
        case 0 =>
          println("[ERROR] Error characteristic information file has no fields")
          throw new Exception("no <ErrorInfo> fields")
        case x if x > 1 =>
          println("[ERROR] Error characteristic information file has multiple fields")
          throw new Exception("multiple <ErrorInfo> fields")
        case _ => // no action needed
      }
      if ((xml \\ "ErrorInfo").head != xml) {
        println("[ERROR] Error characteristic information file has misplaced fields")
        throw new Exception("misplaced <ErrorInfo> fields")
      }

      // Now collect the results for each included operation and its 
      // configurations, if any
      val ret = (xml \ "operation").map { operation =>
        // Get the opcode for the operation
        if ((operation \ "@opcode").isEmpty) {
          throw new Exception("missing opcode")
        }
        val opcode = (operation \@ "opcode")

        // Collect the configurations of the operation and check their 
        // validity, if any
        val confs = (operation \ "conf").map { conf =>
          // Check that the XML has the configuration tag and is a leaf
          if (conf.child.nonEmpty) {
            throw new Exception("non-leaf configuration")
          }

          // Check that the configuration has the proper attributes
          Seq("info", "mred", "sdred")
            .filter (attr => (conf \ s"@$attr").isEmpty)
            .foreach(attr => throw new Exception(s"missing $attr attribute"))

          // Extract the attribute values and verify their validity
          val info = conf \@ "info"
          val mred = (conf \@ "mred").toDoubleOption match {
            case Some(value) => value
            case _ =>
              throw new Exception("invalid mred attribute")
          }
          val sdred = (conf \@ "sdred").toDoubleOption match {
            case Some(value) => value
            case _ =>
              throw new Exception("invalid sdred attribute")
          }

          (info -> (mred, sdred))
        }

        (opcode -> confs.toMap)
      }

      ret.toMap
    }
  }

  /** Write the updated error characteristic information to the storage
   * @param chars the updated error characteristic information in a map
   */
  private def _writeChars(chars: ErrorInfo): Unit = {
    // If the file does not yet exist, create it
    val file = new File(storagePath)
    if (!file.exists()) file.createNewFile()

    // Build the XML from the map
    val xml = <ErrorInfo>
      { chars.map { case (opcode, confs) =>
      <operation opcode={opcode}>
        { confs.map { case (info, (mred, sdred)) =>
        <conf info={info} mred={mred.toString()} sdred={sdred.toString()}/>
        }}
      </operation>
      }}
    </ErrorInfo>

    // Write the XML to the file
    val pp = new PrettyPrinter(Int.MaxValue, 2)
    (new PrintWriter(file))
      .append(pp.format(xml))
      .close()
  }

  /** Combine two maps of error characteristic information
   * @param exst the existing error characteristic information
   * @param upds the generated error characteristic information
   * @return the combined error characteristics with `upds` taking precedence over `exst`
   */
  private def _combineChars(exst: ErrorInfo, upds: ErrorInfo): ErrorInfo = {
    (exst.keys ++ upds.keys).map { opcode =>
      val extConfs = exst.getOrElse(opcode, Map.empty[String, (Double, Double)])
      val updConfs = upds.getOrElse(opcode, Map.empty[String, (Double, Double)])
      (opcode -> (extConfs ++ updConfs))
    }.toMap
  }

  /** Compute the error characteristics of the arithmetic units in the CGRA 
   * that are not covered by the existing information
   * @param apprxOps the list of supported approximate operations
   * @param exst the existing error characteristic information
   * @return the new error characteristic information in a map
   */
  private def _produceChars(apprxOps: Seq[AbstractOperation], exst: ErrorInfo)
    (implicit conf: cgragen.Parameters): ErrorInfo = {
    // Get the set of unique configuration labels for the approximate 
    // operations and filter them with the existing ones
    val needed = apprxOps
      .flatMap(oprtn => _labels(oprtn).map(oprtn -> _))
      .filter { case (oprtn, info) =>
        val opstr = oprtn.op.toString()
        !exst.contains(opstr) || !exst(opstr).contains(info) }

    // Now generate the error characteristics of the missing configurations
    needed.foldLeft(ErrorInfo.empty) { case (acc, (oprtn, info)) =>
      val opstr = oprtn.op.toString()
      if (!acc.contains(opstr) || !acc(opstr).contains(info)) {
        val mInd = info.split('_').last.toInt

        // Run the simulation
        val (mred, sdred) = _characterize(oprtn, mInd)

        // Store the results accordingly
        oprtn.op match {
          case OpAdd | OpSub =>
            val (addstr, substr) = (OpAdd.toString(), OpSub.toString())
            val wAdd = if (!acc.contains(addstr))
              acc + (addstr -> Map(info -> (mred, sdred)))
            else if (!acc(addstr).contains(info))
              acc + (addstr -> (acc(addstr) + (info -> (mred, sdred))))
            else acc
            val wBoth = if (!wAdd.contains(substr))
              wAdd + (substr -> Map(info -> (mred, sdred)))
            else if (!wAdd(substr).contains(info))
              wAdd + (substr -> (wAdd(substr) + (info -> (mred, sdred))))
            else wAdd
            wBoth
          case _ =>
            if (!acc.contains(opstr))
              acc + (opstr -> Map(info -> (mred, sdred)))
            else if (!acc(opstr).contains(info))
              acc + (opstr -> (acc(opstr) + (info -> (mred, sdred))))
            else acc
        }
      } else {
        acc
      }
    }
  }

  /** Record class for collecting characterization results */
  private class SpecRecord {
    var mred : Option[Double] = None
    var sdred: Option[Double] = None
  }

  /** Compute the number of random tests to execute for a module
   * @param size the width of the module operands
   * @return a number of tests
   */
  private def _nTests(size: Int): Int = {
    require(size >= 0)
    1 << (1.25 * scala.math.sqrt(size) + 1).round.toInt
  }

  /** Adder characterization spec
   * @param oprtn the operation
   * @param mInd the approximation mode to characterize
   * @param rec the record to store results in
   * 
   * @note Assumes the adder is always performing unsigned operations.
   */
  private class AdderSpec(oprtn: AbstractOperation, mInd: Int, rec: SpecRecord)
    (implicit conf: cgragen.Parameters) extends AnyFlatSpec with ChiselScalatestTester {
    require(oprtn.op == OpAdd || oprtn.op == OpSub)
    val width       = oprtn.dataSize
    val approxWidth = (width * conf.CGRAApproximationWidthFraction).toInt
    val numModes    = conf.CGRAApproximationModes

    val rng    = new Random(42)
    val mask   = (BigInt(1) << width) - 1
    val mtrcs  = Seq(MRED(), SDRED())
    val nTests = _nTests(width)

    "Inexact adder" should "sum operands" in {
      test(new AdaptiveOFLOCA(width, approxWidth, numModes))
        .withAnnotations(chiselTestAnnos) { dut =>
        // Track the adder's sum output
        val er = new ErrorReporter(track(dut.io.s, mtrcs:_*))

        // Apply a bunch of random inputs and collect the outputs
        dut.io.ctrl.poke(mInd.U)
        (0 until nTests).foreach { _ =>
          val (a, b, cin) = (BigInt(width, rng), BigInt(width, rng), rng.nextBoolean())
          val sum = (a + b + (if (cin) 1 else 0)) & mask
          dut.io.a  .poke(a.U)
          dut.io.b  .poke(b.U)
          dut.io.cin.poke(cin.B)
          dut.clock .step()
          er.sample(Map(dut.io.s -> sum))
        }

        // Finally, register the values of the metrics
        er.query(dut.io.s).foreach {
          case (_: MRED,  Right(mred))  => rec.mred  = Some(mred)
          case (_: SDRED, Right(sdred)) => rec.sdred = Some(sdred)
          case _ => // no action needed
        }
      }
    }
  }

  /** Multiplier characterization spec
   * @param oprtn the operation
   * @param mInd the approximation mode to characterize
   * @param rec the record to store results in
   * 
   * @note Assumes the multiplier is always performing unsigned operations.
   */
  private class MultiplierSpec(oprtn: AbstractOperation, mInd: Int, rec: SpecRecord)
    (implicit conf: cgragen.Parameters) extends AnyFlatSpec with ChiselScalatestTester {
    require(oprtn.op == OpMul)
    val width       = oprtn.dataSize
    val approxWidth = (width * conf.CGRAApproximationWidthFraction).toInt
    val numModes    = conf.CGRAApproximationModes

    val rng    = new Random(42)
    val mask   = (BigInt(1) << width) - 1
    val mtrcs  = Seq(MRED(), SDRED())
    val nTests = _nTests(width)

    "Inexact multiplier" should "multiply operands" in {
      test(new Module {
        val io = IO(new Bundle {
          val ctrl = Input(UInt(log2Up(numModes+1).W))
          val a    = Input(UInt(width.W))
          val b    = Input(UInt(width.W))
          val p    = Output(UInt(width.W))
        })

        val mult = Module(new AdaptiveRadix2Multiplier(width, width, approxWidth, comp=true, numModes=numModes))
        mult.io.ctrl := io.ctrl
        mult.io.a    := io.a
        mult.io.b    := io.b
        io.p := mult.io.p(width-1, 0)
      }).withAnnotations(chiselTestAnnos) { dut =>
        // Track the multiplier's product output
        val er = new ErrorReporter(track(dut.io.p, mtrcs:_*))

        // Apply a bunch of random inputs and collect the outputs
        dut.io.ctrl.poke(mInd.U)
        (0 until nTests).foreach { _ =>
          val (a, b) = (BigInt(width, rng), BigInt(width, rng))
          val prod = (a * b) & mask
          dut.io.a.poke(a.U)
          dut.io.b.poke(b.U)
          dut.clock.step()
          er.sample(Map(dut.io.p -> prod))
        }

        // Finally, register the values of the metrics
        er.query(dut.io.p).foreach {
          case (_: MRED,  Right(mred))  => rec.mred  = Some(mred)
          case (_: SDRED, Right(sdred)) => rec.sdred = Some(sdred)
          case _ => // no action needed
        }
      }
    }
  }

  /** Characterize an approximate arithmetic operation
   * @param oprtn the operation
   * @param mInd the approximation mode to characterize
   * @return a pair of (MRED, SDRED) error characteristics of `oprtn`
   */
  private def _characterize(oprtn: AbstractOperation, mInd: Int)
    (implicit conf: cgragen.Parameters): (Double, Double) = {
    // Collect results in a simple class
    val record = new SpecRecord

    // Run a specific characterizer depending on the given type of operation
    try {
      Console.withOut(new ByteArrayOutputStream) { oprtn.op match {
        case OpAdd | OpSub =>
          run(new AdderSpec(oprtn, mInd, record))
        case OpMul =>
          run(new MultiplierSpec(oprtn, mInd, record))
        case _ => throw new Exception("invalid characterization")
      }}
    } catch {
      case err: Throwable =>
        println(s"[ERROR] Failed to characterize operation (${oprtn.op}) with outputs:")
        println(err.getMessage())
    }

    // Verify that the characterization succeeded
    if (record.mred == None || record.sdred == None) {
      println(s"[ERROR] Characterization results are missing for operation (${oprtn.op})")
      throw new Exception("characterization results missing")
    }
    (record.mred.get, record.sdred.get)
  }

  /** Generate the configuration labels of an operation
   * @param oprtn the operation
   * @return the configuration labels of `oprtn`
   */
  private def _labels(oprtn: AbstractOperation)
    (implicit conf: cgragen.Parameters): Seq[String] = {
    val width       = oprtn.dataSize
    val approxWidth = (width * conf.CGRAApproximationWidthFraction).toInt
    val numModes    = conf.CGRAApproximationModes
    (1 to numModes).map(mInd => s"${width}_${approxWidth}_${numModes}_${mInd}")
  }

  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    // Check that the CGRA has approximate arithmetic operations
    val apprxOps = supportedApprxOps(cgra)
    if (apprxOps.isEmpty) {
      print("Warning! CGRA contains no approximate operations. No more nodes ")
      println("will be marked with error characteristics.")
      return
    }

    // Now perform this pass
    if (conf.CGRADebug)
      println(s"[DEBUG] Marking approximate arithmetic operations in CGRA")

    // Clear any characteristics stored in the approximate arithmetic operations
    apprxOps
      .filter(_.chars.nonEmpty)
      .foreach(oprtn => oprtn.chars.keys.foreach(oprtn.removeChar(_)))

    // Read the existing characteristics
    val extChars = _readChars()

    // Generate any missing characteristics
    val newChars = _produceChars(apprxOps, extChars)

    // Update the characteristics storage
    val updChars = _combineChars(extChars, newChars)
    _writeChars(updChars)

    // Annotate the approximate arithmetic operations with their respective 
    // error characteristics
    apprxOps.foreach { oprtn =>
      _labels(oprtn).foreach { info =>
        val  mInd = info.split('_').last.toInt
        val (mred, sdred) = updChars(oprtn.op.toString())(info)
        oprtn.addChar(mInd, mred, sdred)
      }
    }

    if (conf.CGRADebug) {
      print(s"[DEBUG] Marked a total of (${apprxOps.size}) arithmetic ")
      println("operations with their error characteristics in CGRA")
    }
  }
}
