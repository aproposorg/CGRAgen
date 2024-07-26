package cgragen.hwgen

import cgragen.archparse.PortType._

import cgragen.cgra.AbstractModule
import cgragen.cgra.primitives.{AbstractFunctionUnit, AbstractRegisterFileUnit}

import cgragen.dfgparse.Opcode._

import cgragen.hwgen.primitives.{FunctionUnit, RegisterFile}

import cgragen.TestConfiguration

import chisel3._
import chisel3.util.log2Up

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

/** Common methods for primitive tests */
trait PrimitiveSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfiguration {

  /** To be run after the HWGen conversion tests. Validates that generated 
   * primitive hardware performs the intended operation vs a software model 
   * of their abstract modeling components.
   */

  final val MaxDataSize = 64
  final val NoConfs     = 10
  final val NoOps       = 1000

  // We use a random number generator to generate random configurations of 
  // the test primitives
  val rng = new scala.util.Random(42)
}

class FunctionUnitSpec extends PrimitiveSpec {
  behavior of "Generated function unit"

  import models.FunctionUnitModel

  final val SupportedOps = Seq(OpAdd, OpSub, OpMul, OpDiv, OpAnd, OpOr, OpXor, OpShl, OpShra, OpShrl)
  final val MaxLatency   = 5

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    val numOps = rng.nextInt(SupportedOps.size) + 1
    val ops    = rng.shuffle(SupportedOps).take(numOps)
    val iis    = Seq.fill(numOps)(1) // @todo randomize these when supported!
    val lats   = Seq.fill(numOps) { rng.nextInt(MaxLatency) }

    it should s"work at width $dataSize" in {
      val abstractFU = AbstractFunctionUnit(s"func_${dataSize}b", ops, iis, lats, size=dataSize)

      // Create a software model of the function unit
      val modelFU = new FunctionUnitModel(abstractFU)

      test(new FunctionUnit(abstractFU))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        // Zero-out all inputs to the function unit
        dut.en .poke(true.B)
        dut.clr.poke(false.B)
        dut.io  .ins.elements.keys.foreach { name => dut.io  .ins(name).poke(0.U) }
        dut.ctrl.ins.elements.keys.foreach { name => dut.ctrl.ins(name).poke(0.U) }

        modelFU.en .poke(true)
        modelFU.clr.poke(false)

        // Execute a number of randomly selected operations
        (0 until NoOps).foreach { _ =>
          // Generate the random inputs and pass them to the DUT
          val dinA = BigInt(dataSize, rng)
          val dinB = BigInt(dataSize, rng)
          val sel = if (ops.size > 1) Some(rng.nextInt(ops.size)) else None
          
          val inAPort = "in_a"
          dut.io.ins(inAPort).poke(dinA.U)
          modelFU.io.ins.poke(inAPort, dinA)

          val inBPort = "in_b"
          dut.io.ins(inBPort).poke(dinB.U)
          modelFU.io.ins.poke(inBPort, dinB)

          val selPort = "select"
          sel match {
            case Some(selIn) =>
              dut.ctrl.ins(selPort).poke(selIn.U)
              modelFU.ctrl.ins.poke(selPort, selIn)
            case _ =>
          }

          // Advance the simulation time
          dut.clock.step()
          modelFU.step()

          // Check that the output matches the model
          if (sel != None && ops(sel.get) != OpDiv)
            dut.io.outs("out").expect(modelFU.io.outs.peek("out").U)
        }
      }
    }
  }
}

class RegisterFileSpec extends PrimitiveSpec {
  behavior of "Generated register file"

  import models.RegisterFileModel

  rng.shuffle(1 to MaxDataSize).take(NoConfs).foreach { dataSize =>
    val numInputs  = rng.nextInt(4) + 1
    val numOutputs = rng.nextInt(4) + 1
    val numRegsLg2 = rng.nextInt(5) + 1

    it should s"work with $numInputs inputs, $numOutputs outputs, ${1 << numRegsLg2} registers at width $dataSize" in {
      val rfName = s"rf_in${numInputs}_out${numOutputs}_reg${1 << numRegsLg2}_${dataSize}b"
      val abstractRF = AbstractRegisterFileUnit(rfName, numInputs, numOutputs, numRegsLg2, dataSize)

      // Create a software model of the register file
      val modelRF = new RegisterFileModel(abstractRF)

      test(new RegisterFile(abstractRF))
        .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        // Zero-out all inputs to the register file
        dut.en .poke(true.B)
        dut.clr.poke(false.B)
        dut.io  .ins.elements.keys.foreach { name => dut.io  .ins(name).poke(0.U) }
        dut.ctrl.ins.elements.keys.foreach { name => dut.ctrl.ins(name).poke(0.U) }

        modelRF.en .poke(true)
        modelRF.clr.poke(false)

        // Execute a number of random read-write operations
        (0 until NoOps).foreach { _ =>
          // Generate the random inputs and pass them to the DUT
          val dins     = Array.fill(numInputs)  { BigInt(dataSize,   rng) }
          val addrIns  = Array.fill(numInputs)  { BigInt(numRegsLg2, rng) }
          val wes      = Array.fill(numInputs)  { rng.nextBoolean()       }
          val addrOuts = Array.fill(numOutputs) { BigInt(numRegsLg2, rng) }
          (0 until numInputs).foreach { in =>
            val inPort = s"in$in"
            dut.io.ins(inPort).poke(dins(in).U)
            modelRF.io.ins.poke(inPort, dins(in))

            val addrInPort = s"addr_in$in"
            dut.ctrl.ins(addrInPort).poke(addrIns(in).U)
            modelRF.ctrl.ins.poke(addrInPort, addrIns(in))

            val weInPort = s"WE$in"
            dut.ctrl.ins(weInPort).poke(wes(in).B)
            modelRF.ctrl.ins.poke(weInPort, if (wes(in)) BigInt(1) else BigInt(0))
          }
          (0 until numOutputs).foreach { out =>
            val addrOutPort = s"addr_out$out"
            dut.ctrl.ins(addrOutPort).poke(addrOuts(out).U)
            modelRF.ctrl.ins.poke(addrOutPort, addrOuts(out))
          }

          // Advance the simulation time
          dut.clock.step()
          modelRF.step()

          // Check that the outputs match the model
          (0 until numOutputs).foreach { out =>
            val outPort = s"out$out"
            dut.io.outs(outPort).expect(modelRF.io.outs.peek(outPort).U)
          }
        }
      }
    }
  }
}

/** Various modeling utilities for the tests above */
private[hwgen] object models {
  class PortRecordModel(elts: Seq[String] = Seq.empty[String]) {
    private val _initialized = mutable.HashMap(elts.map(_ -> false):_*)
    private val _elements    = mutable.HashMap(elts.map(_ -> BigInt(0)):_*)
    def elements: Map[String, BigInt] = _elements.toMap

    /** Set a value in the record
     * @param elt the record element to set
     * @param value the value to set
     */
    def poke(elt: String, value: BigInt): Unit = {
      require(_elements.contains(elt))
      _initialized(elt) = true
      _elements(elt)    = value
    }

    /** Peek and return a value in the record
     * @param elt the record element to return
     * @return the value of the element `elt`
     */
    def peek(elt: String): BigInt = {
      require(_elements.contains(elt) && _initialized(elt))
      _elements(elt)
    }

    /** Return whether this record model is empty */
    def isEmpty: Boolean = _elements.isEmpty

    /** Return whether this record model is non-empty */
    def nonEmpty: Boolean = !isEmpty
  }

  class TopLevelPortModel[T] {
    private var _initialized = false
    private var _value       = null.asInstanceOf[T]

    /** Set the value of the port
     * @param value the value to set
     */
    def poke(value: T): Unit = {
      _initialized = true
      _value = value
    }

    /** Peek and return the value of the port
     * @return the value of the port
     */
    def peek(): T = {
      require(_initialized)
      _value
    }
  }

  case class IOModel(ins: PortRecordModel, outs: PortRecordModel) {
    /** Return whether this IO model is empty */
    def isEmpty: Boolean = ins.isEmpty && outs.isEmpty

    /** Return whether this IO model is non-empty */
    def nonEmpty: Boolean = !isEmpty
  }

  abstract class PrimitiveModel(mod: AbstractModule) {
    /** Fields start ************************************************************/
    // To match the description of the hardware modules, the model defines the 
    // same interfaces but with a somewhat reduced functionality
    val io = {
      val ins = mod.ports
        .filter { case (_, port) =>
          port.pt == PortInput && !mod.configCells.exists(_._2.port == port) }
        .map(_._1).toSeq
      val outs = mod.ports
        .filter { case (_, port) => port.pt == PortOutput }
        .map(_._1).toSeq
      IOModel(new PortRecordModel(ins), new PortRecordModel(outs))
    }
    val ctrl = {
      val ins = mod.configCells
        .map { case (_, cell) => cell.port.name }.toSeq
      IOModel(new PortRecordModel(ins), new PortRecordModel())
    }

    // Enable and clear signals for the data path
    val en  = new TopLevelPortModel[Boolean]()
    val clr = new TopLevelPortModel[Boolean]()
    /** Fields end **************************************************************/

    /** Various methods start ***************************************************/
    /** Progress time of the model by a number of clock cycles
     * @param cycles the number of cycles to progress time of the model
     * 
     * @note Inheriting models must implement
     */
    def step(cycles: Int = 1): Unit

    /** Return this primitive model as a string */
    override def toString(): String = {
      val bs = new StringBuilder(s"Model of primitive (${mod.name}) with ports:\n")
      def portsToString(ports: Iterable[String]): String = ports.map(p => s"    $p").mkString("\n")

      if (io.ins.nonEmpty)
        bs ++= s"  Inputs:\n${portsToString(io.ins.elements.keys)}\n"
      if (io.outs.nonEmpty)
        bs ++= s"  Outputs:\n${portsToString(io.outs.elements.keys)}\n"
      if (ctrl.ins.nonEmpty)
        bs ++= s"  Control inputs:\n${portsToString(ctrl.ins.elements.keys)}\n"
      if (ctrl.outs.nonEmpty)
        bs ++= s"  Control outputs:\n${portsToString(ctrl.outs.elements.keys)}"

      bs.mkString
    }
    /** Various methods end *****************************************************/
  }

  class FunctionUnitModel(mod: AbstractFunctionUnit) extends PrimitiveModel(mod) {
    // Keep track of operations and results in a queue
    private val _maxLat = mod.operations.map(_.lat).max + 1
    private val _resQueue = Array.fill(_maxLat)(Array.fill(mod.operations.size)(BigInt(0)))

    // For simpler reference, extract the input and output port names here
    private val _select = if (mod.ports.contains("select")) Some("select") else None

    private val _mask = (BigInt(1) << mod.dataSize) - 1
    private val _sftMask = (BigInt(1) << log2Up(mod.dataSize)) - 1
    private val _sext = BigInt(-1) << mod.dataSize
    private def sext(num: BigInt): BigInt = {
      (if (num.testBit(mod.dataSize-1)) _sext else BigInt(0)) | num
    }

    def step(cycles: Int = 1): Unit = (0 until cycles).foreach { _ =>
      /** Progressing time in the function unit involves
       * 
       * 1. performing all operations and inserting their results into 
       *    the queue of results
       * 2. shifting the queue by one position
       * 3. setting the output to the head of the queue
       */

      // Step 1: performing all operations
      val a = io.ins.peek("in_a")
      val b = io.ins.peek("in_b")
      mod.operations.zipWithIndex.foreach { case (oprtn, i) => // @todo parameterize ii when supported!
        val res = (oprtn.op match {
          case OpAdd  => a + b
          case OpSub  => a - b
          case OpMul  => a * b
          case OpDiv  => if (b != 0) a / b else BigInt(-1)
          case OpAnd  => a & b
          case OpOr   => a | b
          case OpXor  => a ^ b
          case OpShl  => a       << (b & _sftMask).intValue
          case OpShra => sext(a) >> (b & _sftMask).intValue
          case OpShrl => a       >> (b & _sftMask).intValue
          case _ =>
            throw new Exception(s"cannot generate hardware for operation ${oprtn.op}")
        }) & _mask
        if (en.peek()) {
          _resQueue(oprtn.lat)(i) = res
          if (oprtn.lat == 0) _resQueue(oprtn.lat+1)(i) = res // to ensure combinational connections
        }
      }

      // Step 2: shifting the queue or resetting it
      if (clr.peek()) {
        (0 until _maxLat).foreach { c =>
          (0 until mod.operations.size).foreach(i => _resQueue(c)(i) = BigInt(0))
        }
      } else if (en.peek()) {
        (1 until _maxLat).foreach { c =>
          (0 until mod.operations.size).foreach(i => _resQueue(c-1)(i) = _resQueue(c)(i))
        }
        (0 until mod.operations.size).foreach(i => _resQueue(_maxLat-1)(i) = BigInt(0))
      }

      // Step 3: setting the output
      val sel = (_select match {
        case Some(name) => ctrl.ins.peek(name)
        case _ => BigInt(0)
      }).intValue
      assert(sel < mod.operations.size)
      io.outs.poke("out", _resQueue(0)(sel))
    }
  }

  class RegisterFileModel(mod: AbstractRegisterFileUnit) extends PrimitiveModel(mod) {
    // Keep track of register values in an array
    private val _regs = Array.fill(1 << mod.numRegsLg2)(BigInt(0))

    def step(cycles: Int = 1): Unit = {
      /** Progressing time in the register file involves
       * 
       * 1. setting the values of the write-enabled registers
       * 2. setting the outputs of the enabled ports
       */

      // Step 1: setting or resetting the register values
      if (clr.peek()) {
        (0 until _regs.size).foreach(_regs(_) = BigInt(0))
      } else if (en.peek()) {
        (0 until mod.numInputs).foreach { in =>
          val addr = ctrl.ins.peek(s"addr_in$in").intValue
          assert(addr < _regs.size)
          val we = ctrl.ins.peek(s"WE$in").intValue
          assert(we == 0 || we == 1)
          if (we == 1) _regs(addr) = io.ins.peek(s"in$in")
        }
      }

      // Step 2: setting the outputs
      (0 until mod.numOutputs).foreach { out =>
        val addr = ctrl.ins.peek(s"addr_out$out").intValue
        assert(addr < _regs.size)
        io.outs.poke(s"out$out", _regs(addr))
      }
    }
  }
}
