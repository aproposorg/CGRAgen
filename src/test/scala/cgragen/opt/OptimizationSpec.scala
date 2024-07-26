package cgragen.opt

import cgragen.{Parameters, TestConfiguration}

import cgragen.archparse.{Architecture, Connection, Template}

import cgragen.cgra.CGRA

import cgragen.dfgparse.DFG

import cgragen.opt.passes._

import java.io.{File, PrintWriter}

import scala.language.implicitConversions

import scala.xml.{Attribute, Elem, MetaData, Text}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

/** Optimization tests */
trait OptimizationSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Optimization passes"

  /** These tests do not require any particular CGRA */
  final val cgra = CGRA(Architecture(fourInFourOutArch))(params.copy(cgraInferTopLevelIO = true))
}

class DeadNodeRemovalSpec extends OptimizationSpec {
  val locParams = params.copy(dfgRemoveDeadNodes = true)
  val passes    = getOptPasses()(locParams)

  it should "remove dead nodes and edges when requested" in {
    val file = File.createTempFile("valid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |in0[opcode=input];
                |in1[opcode=input];
                |add[opcode=add];
                |in0->add[operand=0];
                |in1->add[operand=1];
                |}
                |""".stripMargin)
      .close()
    val emptyG = DFG(file)
    passes(emptyG, cgra)
    emptyG.nodes should have length 0
    emptyG.edges should have length 0


    (new PrintWriter(file))
      .append("""digraph G {
                |in0[opcode=input];
                |in1[opcode=input];
                |add[opcode=add];
                |in0->add[operand=0];
                |in1->add[operand=1];
                |out[opcode=output];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val simpleG = DFG(file)
    passes(simpleG, cgra)
    simpleG.nodes.map(_.name) should (have size 4 and
      contain allElementsOf Seq("in0", "in1", "add", "out"))
    simpleG.edges.map(_.src.name) should (have size 3 and
      contain allElementsOf Seq("in0", "in1", "add"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in0[opcode=input];
                |in1[opcode=input];
                |add[opcode=add];
                |in0->add[operand=0];
                |in1->add[operand=1];
                |out[opcode=output];
                |mul[opcode=mul];
                |nop[opcode=const];
                |const[opcode=const];
                |add->mul[operand=0];
                |const->mul[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val extendedG = DFG(file)
    passes(extendedG, cgra)
    extendedG.nodes.map(_.name) should (have size 4 and
      contain allElementsOf Seq("in0", "in1", "add", "out"))
    extendedG.edges.map(_.src.name) should (have size 3 and
      contain allElementsOf Seq("in0", "in1", "add"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in0[opcode=input];
                |in1[opcode=input];
                |add0[opcode=add];
                |in0->add0[operand=0];
                |in1->add0[operand=1];
                |out0[opcode=output];
                |mul[opcode=mul];
                |const[opcode=const];
                |add0->mul[operand=0];
                |const->mul[operand=1];
                |add0->out0[operand=0];
                |out1[opcode=output];
                |add1[opcode=add];
                |add0->add1[operand=1];
                |mul->add1[operand=0];
                |add1->out1[operand=0];
                |}
                |""".stripMargin)
      .close()
    val largeG = DFG(file)
    passes(largeG, cgra)
    largeG.nodes.map(_.name) should (
      have size 8 and
      contain allElementsOf Seq("in0", "in1", "add0", "add1", "out0", "out1", "const", "mul"))
    largeG.edges.map(_.src.name) should (
      have size 8 and
      contain allElementsOf Seq("in0", "in1", "add0", "add1", "const", "mul"))

    file.delete()
  }
}

class ConstantPropagationSpec extends OptimizationSpec {
  val locParams = params.copy(dfgPropagateConstants = true, cgraInferTopLevelIO = true)
  val passes    = getOptPasses()(locParams)

  it should "propagate constant zeros when requested" in {
    val file = File.createTempFile("valid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=1234];
                |add[opcode=add];
                |out[opcode=output];
                |in->add[operand=0];
                |cst->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val simpleG = DFG(file)
    passes(simpleG, cgra)
    simpleG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst", "add", "out"))
    simpleG.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst", "add"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const]; /** implicit constant 0 */
                |add[opcode=add];
                |out[opcode=output];
                |in->add[operand=0];
                |cst->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val impl0G = DFG(file)
    passes(impl0G, cgra)
    impl0G.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("in", "out"))
    impl0G.edges.map(_.src.name) should (
      have size 1 and
      contain ("in"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in0[opcode=input];
                |in1[opcode=input];
                |cst[opcode=const]; /** implicit constant 0 */
                |add[opcode=add];
                |mul[opcode=mul];
                |out0[opcode=output];
                |out1[opcode=output];
                |in0->add[operand=0];
                |cst->add[operand=1];
                |in1->mul[operand=0];
                |cst->mul[operand=1];
                |add->out0[operand=0];
                |mul->out1[operand=0];
                |}
                |""".stripMargin)
      .close()
    val impl0LG = DFG(file)
    passes(impl0LG, cgra)
    impl0LG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in0", "out0", "mul_const0", "out1"))
    impl0LG.edges.map(_.src.name) should (
      have size 2 and
      contain allElementsOf Seq("in0", "mul_const0"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst0[opcode=const]; /** implicit constant 0 */
                |and[opcode=and];
                |cst1[opcode=const value=1234];
                |mul[opcode=mul];
                |in->and[operand=0];
                |cst0->and[operand=1];
                |and->mul[operand=0];
                |cst1->mul[operand=1];
                |out[opcode=output];
                |mul->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val prop0G = DFG(file)
    passes(prop0G, cgra)
    prop0G.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("mul_const0", "out"))
    prop0G.edges.map(_.src.name) should (
      have size 1 and
      contain ("mul_const0"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const]; /** implicit constant 0 */
                |sub[opcode=sub];
                |in->sub[operand=0];
                |cst->sub[operand=1];
                |out[opcode=output];
                |sub->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val sub0G = DFG(file)
    passes(sub0G, cgra)
    sub0G.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("in", "out"))
    sub0G.edges.map(_.src.name) should (
      have size 1 and
      contain ("in"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const]; /** implicit constant 0 */
                |sub[opcode=sub];
                |cst->sub[operand=0];
                |in->sub[operand=1];
                |out[opcode=output];
                |sub->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val subG = DFG(file)
    passes(subG, cgra)
    subG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst", "sub", "out"))
    subG.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst", "sub"))


    (new PrintWriter(file))
      .append("""digraph G {
                |cst0[opcode=const]; /** implicit constant 0 */
                |cst1[opcode=const value=1234];
                |sub[opcode=sub];
                |cst0->sub[operand=0];
                |cst1->sub[operand=1];
                |out[opcode=output];
                |sub->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val subCG = DFG(file)
    passes(subCG, cgra)
    subCG.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("cst1", "out"))
    subCG.edges.map(_.src.name) should (
      have size 1 and
      contain ("cst1"))
    subCG.nodes
      .collectFirst { case const if const.name == "cst1" => const }
      .foreach { _.attrs("value").toInt should equal (-1234) }


    (new PrintWriter(file))
      .append("""digraph G {
                |cst0[opcode=const]; /** implicit constant 0 */
                |cst1[opcode=const value=1234];
                |sub[opcode=sub];
                |cst1->sub[operand=0];
                |cst0->sub[operand=1];
                |out[opcode=output];
                |sub->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val subC0G = DFG(file)
    passes(subC0G, cgra)
    subC0G.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("cst1", "out"))
    subC0G.edges.map(_.src.name) should (
      have size 1 and
      contain ("cst1"))
    subC0G.nodes
      .collectFirst { case const if const.name == "cst1" => const }
      .foreach { _.attrs("value").toInt should equal (1234) }


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst0[opcode=const]; /** implicit constant 0 */
                |mul[opcode=mul];
                |in->mul[operand=0];
                |cst0->mul[operand=1];
                |cst1[opcode=const value=1234];
                |sub[opcode=sub];
                |mul->sub[operand=0];
                |cst1->sub[operand=1];
                |out[opcode=output];
                |sub->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val subM0G = DFG(file)
    passes(subM0G, cgra)
    subM0G.nodes.map(_.name) should (have size 2 and
      contain allElementsOf Seq("cst1", "out"))
    subM0G.edges.map(_.src.name) should (have size 1 and
      contain ("cst1"))
    subM0G.nodes
      .collectFirst { case const if const.name == "cst1" => const }
      .foreach { _.attrs("value").toInt should equal (-1234) }


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const]; /** implicit constant 0 */
                |shl[opcode=shl];
                |in->shl[operand=0];
                |cst->shl[operand=1];
                |out[opcode=output];
                |shl->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val shPIG = DFG(file)
    passes(shPIG, cgra)
    shPIG.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("in", "out"))
    shPIG.edges.map(_.src.name) should (
      have size 1 and
      contain ("in"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const]; /** implicit constant 0 */
                |shra[opcode=shra];
                |cst->shra[operand=0];
                |in->shra[operand=1];
                |out[opcode=output];
                |shra->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val shP0G = DFG(file)
    passes(shP0G, cgra)
    shP0G.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("shra_const0", "out"))
    shP0G.edges.map(_.src.name) should (
      have size 1 and
      contain ("shra_const0"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in0[opcode=input];
                |in1[opcode=input];
                |add0[opcode=add];
                |in0->add0[operand=0];
                |in1->add0[operand=1];
                |mul[opcode=mul];
                |const[opcode=const];
                |add0->mul[operand=0];
                |const->mul[operand=1];
                |out0[opcode=output];
                |add0->out0[operand=0];
                |add1[opcode=add];
                |add0->add1[operand=1];
                |mul->add1[operand=0];
                |out1[opcode=output];
                |add1->out1[operand=0];
                |}
                |""".stripMargin)
      .close()
    val shrdFIG = DFG(file)
    passes(shrdFIG, cgra)
    shrdFIG.nodes.map(_.name) should (
      have size 5 and
      contain allElementsOf Seq("in0", "in1", "add0", "out0", "out1"))
    shrdFIG.edges.map(_.src.name) should (
      have size 4 and
      contain allElementsOf Seq("in0", "in1", "add0"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst0[opcode=const];
                |cst1[opcode=const value=1234];
                |mul[opcode=mul];
                |in->mul[operand=0];
                |cst0->mul[operand=1];
                |add[opcode=add];
                |cst1->add[operand=0];
                |mul->add[operand=1];
                |out[opcode=output];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val twoCnstG = DFG(file)
    passes(twoCnstG, cgra)
    twoCnstG.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("cst1", "out"))
    twoCnstG.edges.map(_.src.name) should (
      have size 1 and
      contain ("cst1"))

    file.delete()
  }

  it should "propagate constant powers-of-two when requested and possible" in {
    val file = File.createTempFile("valid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=8];
                |mul[opcode=mul];
                |in->mul[operand=0];
                |cst->mul[operand=1];
                |out[opcode=output];
                |mul->out[operand=0];
                |}
                |""".stripMargin)
      .close()

    // Test for a CGRA with shifters
    val pow2CG = DFG(file)
    passes(pow2CG, cgra)
    pow2CG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst_lg", "mul_shl", "out"))
    pow2CG.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst_lg", "mul_shl"))
    pow2CG.nodes
      .collectFirst { case const if const.name == "cst_lg" => const }
      .foreach { _.attrs("value").toInt should equal (3) }


    // Test for a CGRA without shifters
    val pow2CGWOshl = DFG(file)
    val archWOshl   = Architecture(fourInFourOutArch)

    // Extract the original template
    val orig = archWOshl.modTemplates.values.head

    // Create a copy of it with the function unit altered
    val copy = new Template(orig.name, orig.dataSize)
    orig.connections.foreach { case Connection(toType, to, fromType, from, sext) =>
      copy.addConnection(toType, to, fromType, from, sext)
    }
    orig.ports.foreach { case (portName, (pt, size)) =>
      copy.addPort(portName, pt, size)
    }
    orig.primitives.foreach {
      case ("FuncUnit", primArgs) =>
        copy.addPrimitive("FuncUnit", primArgs.filterNot(_._1 == "ops") + ("ops" -> "add sub"))
      case (primType, primArgs) =>
        copy.addPrimitive(primType, primArgs)
    }
    orig.subModules.foreach { case (name, mt) =>
      copy.addSubModule(name, mt)
    }
    orig.wires.foreach { case (wireName, portName) =>
      copy.addWire(wireName, portName)
    }

    // Override the template in the architecture
    archWOshl.addTemplate(orig.name, copy)
    archWOshl.subModules.foreach { case (name, _) =>
      archWOshl.addSubModule(name, orig.name)
    }

    val cgraWOshl = CGRA(archWOshl)(locParams)
    passes(pow2CGWOshl, cgraWOshl)
    pow2CGWOshl.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst", "mul", "out"))
    pow2CGWOshl.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst", "mul"))


    file.delete()
  }
}

class MaxFanoutSpec extends OptimizationSpec {
  val locParams = params.copy(dfgLimitFanout = true, dfgMaxFanout = 2)
  val passes    = getOptPasses()(locParams)

  it should "do nothing to a low-fanout graph" in {
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
    passes(sameG, cgra)(locParams)
    sameG.nodes.map(_.name) should (
      have size 4 and
      contain allElementsOf Seq("in", "cst", "mul", "out"))
    sameG.edges.map(_.src.name) should (
      have size 3 and
      contain allElementsOf Seq("in", "cst", "mul"))
    
    file.delete()
  }

  it should "recursively limit fanout" in {
    val file = File.createTempFile("valid", ".dot")

    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=1234];
                |mul[opcode=mul];
                |in->mul[operand=0];
                |cst->mul[operand=1];
                |add[opcode=add];
                |mul->add[operand=0];
                |cst->add[operand=1];
                |xor[opcode=xor];
                |add->xor[operand=0];
                |cst->xor[operand=1];
                |add2[opcode=add];
                |cst->add2[operand=0];
                |xor->add2[operand=1];
                |out[opcode=output];
                |add2->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val oneG = DFG(file)
    passes(oneG, cgra)(locParams)
    oneG.nodes.map(_.name) should (
      have size 8 and
      contain allElementsOf
        Seq("in", "cst", "cst_fo0", "mul", "add", "xor", "add2", "out"))
    oneG.edges.map(_.src.name) should (
      have size 9 and
      contain allElementsOf
        Seq("in", "cst", "cst_fo0", "mul", "add", "xor", "add2"))


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=1234];
                |shl[opcode=shl];
                |in->shl[operand=0];
                |cst->shl[operand=1];
                |mul[opcode=mul];
                |in->mul[operand=0];
                |shl->mul[operand=1];
                |add[opcode=add];
                |mul->add[operand=0];
                |shl->add[operand=1];
                |xor[opcode=xor];
                |add->xor[operand=0];
                |shl->xor[operand=1];
                |add2[opcode=add];
                |cst->add2[operand=0];
                |xor->add2[operand=1];
                |out[opcode=output];
                |add2->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val twoG = DFG(file)
    passes(twoG, cgra)(locParams)
    twoG.nodes.map(_.name) should (
      have size 10 and
      contain allElementsOf
        Seq("in", "cst", "cst_fo0", "shl", "shl_fo0",
            "mul", "add", "xor", "add2", "out"))
    twoG.edges.map(_.src.name) should (
      have size 13 and
      contain allElementsOf
        Seq("in", "cst", "cst_fo0", "shl", "shl_fo0",
            "mul", "add", "xor", "add2"))

    file.delete()
  }
}
