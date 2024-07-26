package cgragen.opt

import cgragen.TestConfiguration

import cgragen.archparse.Architecture

import cgragen.cgra.CGRA

import cgragen.dfgparse.{DFG, DFGNode, isArith, isLogic}, DFG.fanInCone
import cgragen.dfgparse.Opcode._

import cgragen.opt.getOptPasses
import cgragen.opt.passes._

import scala.io.Source

import java.io.{File, PrintWriter}
import java.nio.file.{Files, FileSystems, Path}

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

/** Approximation tests */
trait ApproximationSpec extends AnyFlatSpec with TestConfiguration

class NodeMarkingSpec extends ApproximationSpec {
  behavior of "Approximable node marking pass"
  val locParams = params.copy(cgraInferTopLevelIO = true, approximateArithmetic = true)
  val cgra      = CGRA(Architecture(fourInFourOutArch))(locParams)
  val passes    = getOptPasses()(locParams)

  it should "leave a non-approximable DFG unchanged" in {
    val file = File.createTempFile("valid", ".dot")

    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=1234];
                |mul[opcode=mul];
                |in->mul[operand=0];
                |cst->mul[operand=1];
                |out[opcode=output];
                |mul->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val sameG = DFG(file)
    passes(sameG, cgra)
    sameG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst", "mul", "out"))
    all (sameG.nodes.map(_.attrs.contains("approx"))) should equal (false)
    sameG.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst", "mul"))

    file.delete()
  }

  it should "de-mark incorrectly marked output nodes" in {
    val file = File.createTempFile("valid", ".dot")

    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=1234];
                |mul[opcode=mul];
                |in->mul[operand=0];
                |cst->mul[operand=1];
                |out[opcode=output approx];
                |mul->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val incG = DFG(file)
    passes(incG, cgra)
    incG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst", "mul", "out"))
    all (incG.nodes.map(_.attrs.contains("approx"))) should equal (false)
    incG.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst", "mul"))

    file.delete()
  }

  it should "mark approximable nodes in a single-output DFG" in {
    // Load the 2x2 convolution graph and mark its output node approximable
    val snglOutG = DFG("./src/test/resources/dfgparse/conv2x2.dot")
    snglOutG.nodes
      .filter(_.opcode == OpOutput)
      .foreach(_.addAttr("approx", ".1"))
    passes(snglOutG, cgra)

    val logicOrAriths = snglOutG.nodes
      .filter(node => isArith(node.opcode) || isLogic(node.opcode))
    all (logicOrAriths.map(_.attrs.contains("approx"))) should equal (true)
    val others = snglOutG.nodes
      .filter(node => !logicOrAriths.contains(node) && node.opcode != OpOutput)
    all (others.map(_.attrs.contains("approx"))) should equal (false)
  }

  it should "mark approximable nodes in a multi-output DFG" in {
    // Load the dct4p graph and mark two of its output nodes approximable
    val mltOutG = DFG("./src/test/resources/dfgparse/dct4p.dot")
    val apprxmblOuts = {
      val outs = mltOutG.nodes
        .filter(_.opcode == OpOutput)
      outs.take(outs.size / 2)
    }
    apprxmblOuts.foreach(_.addAttr("approx", ".1"))
    passes(mltOutG, cgra)

    // Find the unique fan-in cone of each approximable output node
    val apprxmbls = {
      val apprxmblFIs = apprxmblOuts
        .foldLeft(Set.empty[DFGNode]) { case (acc, out) => acc ++ fanInCone(out, mltOutG) }
        .filter(node => isArith(node.opcode) || isLogic(node.opcode))
      val nonApprxmblFIs = mltOutG.nodes
        .filter(node => node.opcode == OpOutput && !apprxmblOuts.contains(node))
        .foldLeft(Set.empty[DFGNode]) { case (acc, out) => acc ++ fanInCone(out, mltOutG) }
      apprxmblFIs -- nonApprxmblFIs
    }
    all (apprxmbls.map(_.attrs.contains("approx"))) should equal (true)
    val others = mltOutG.nodes
      .filter(node => !apprxmbls.contains(node) && node.opcode != OpOutput)
    all (others.map(_.attrs.contains("approx"))) should equal (false)
  }
}

class ErrorEstimationSpec extends ApproximationSpec with BeforeAndAfterAll {
  behavior of "Error estimation pass"
  val storagePath = EstimateErrorsPass.storagePath
  val locParams   = params.copy(
    dataSize = 12,  // limit simulation time with an artificially low bit-width
    cgraApproximationModes = 2,
    cgraApproximationWidthFraction = .4
  )
  val arch   = Architecture(apprxmblArch)(locParams)
  val dfg    = new DFG
  val passes = getOptPasses()(locParams.copy(approximateArithmetic = true))

  /** Read the existing characterization data, if any */
  def readChars(): String = if (!(new File(storagePath)).exists()) "" else {
    val src   = Source.fromFile(storagePath)
    val cntnt = src.getLines().mkString("\n")
    src.close()
    cntnt
  }

  /** Generate a regex string for an ErrorInfo operation
   * @param opcode the opcode of the operation
   * @return a regex string matching an ErrorInfo operation
   */
  def operationRegex(opcode: String): String = s"""<operation\\s+opcode="$opcode"/?>"""

  /** Generate a regex string for an ErrorInfo configuration
   * @param info the label of the configuration
   * @return a regex string matching an ErrorInfo configuration
   */
  def configurationRegex(info: String): String = {
    s"""<conf\\s+info="$info"\\s+mred="[0-9\\.eE-]+"\\s+sdred="[0-9\\.eE-]+"/>"""
  }

  // Keep the existing error estimation data for restoration following 
  // this test to ensure proper functionality of the main flow
  val existing = readChars()

  // Something known to check that the characteristics file is updated properly
  val knownLabels = Seq("16_8_2_1", "16_8_2_2", "32_16_1_1")
  val knownErrors = <ErrorInfo>
    <operation opcode="add">
      <conf info="16_8_2_1" mred="1.21e-7" sdred="6.31e-8"/>
      <conf info="16_8_2_2" mred="5.42e-5" sdred="2.36e-6"/>
    </operation>
    <operation opcode="mul">
      <conf info="32_16_1_1" mred="8.57e-11" sdred="5.62e-13"/>
    </operation>
  </ErrorInfo>

  // Remove the file with existing error characteristics
  val path = (new File(storagePath)).toPath()
  override def beforeAll(): Unit = {
    Files.deleteIfExists(path)
  }

  it should "estimate errors with no existing file" in {
    // Run the pass only for a manually marked adder
    val cgra = CGRA(arch)(locParams)
    passes(dfg, cgra)(locParams)

    // Read the file content and validate that it contains the right entries
    val chars = readChars()
    chars should (
      include regex (operationRegex("add")) and
      include regex (operationRegex("sub")))
    val labels = {
      val width       = locParams.DataSize
      val approxWidth = (width * locParams.CGRAApproximationWidthFraction).toInt
      val numModes    = locParams.CGRAApproximationModes
      (1 to numModes).map(mInd => s"${width}_${approxWidth}_${numModes}_${mInd}")
    }
    labels.foreach { info =>
      chars should include regex (configurationRegex(info)) }
  }

  it should "estimate errors of adders and subtractors" in {
    (new PrintWriter(new File(storagePath)))
      .append(knownErrors.toString())
      .close()

    // Run the pass only for a manually marked adder
    val cgra = CGRA(arch)(locParams)
    passes(dfg, cgra)(locParams)

    // Read the file content and validate that it contains the right entries
    val chars = readChars()
    chars should (
      include regex (operationRegex("add")) and
      include regex (operationRegex("sub")) and
      include regex (operationRegex("mul")))
    knownLabels.foreach { info =>
      chars should include regex (configurationRegex(info)) }
    val labels = {
      val width       = locParams.DataSize
      val approxWidth = (width * locParams.CGRAApproximationWidthFraction).toInt
      val numModes    = locParams.CGRAApproximationModes
      (1 to numModes).map(mInd => s"${width}_${approxWidth}_${numModes}_${mInd}")
    }
    labels.foreach { info =>
      val rgx = configurationRegex(info).r
      rgx.findAllMatchIn(chars) should have size 2 } // add and sub
  }

  it should "estimate errors of multipliers" in {
    (new PrintWriter(new File(storagePath)))
      .append(knownErrors.toString())
      .close()

    // Run the pass for all arithmetic units in a CGRA
    val cgra = CGRA(arch)(locParams.copy(approximateArithmetic = true))
    passes(dfg, cgra)(locParams)

    // Read the file content and validate that it contains the right entries
    val chars = readChars()
    chars should (
      include regex (operationRegex("add")) and
      include regex (operationRegex("sub")) and
      include regex (operationRegex("mul")))
    knownLabels.foreach { info =>
      chars should include regex (configurationRegex(info)) }
    val labels = {
      val width       = locParams.DataSize
      val approxWidth = (width * locParams.CGRAApproximationWidthFraction).toInt
      val numModes    = locParams.CGRAApproximationModes
      (1 to numModes).map(mInd => s"${width}_${approxWidth}_${numModes}_${mInd}")
    }
    labels.foreach { info =>
      val rgx = configurationRegex(info).r
      rgx.findAllMatchIn(chars) should have size 3 } // add, sub, and mul
  }

  // Clean up by restoring the original file
  override def afterAll(): Unit = {
    if (existing.nonEmpty) {
      (new PrintWriter(new File(storagePath)))
        .append(existing)
        .close()
    }
  }
}
