package cgragen.dfgparse

import cgragen.{Parameters, TestConfiguration}

import cgragen.dfgparse.DFG._
import cgragen.dfgparse.Opcode._

import java.io.{File, PrintWriter}

import scala.util.parsing.combinator._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import cgragen.dfgparse.DFG.fanInCone

/** Reference parser */
private[dfgparse] object reference extends RegexParsers {
  def graph: Parser[List[ParserElement]] = {
    "digraph" ~ id.? ~ "{" ~> rep(nodeStmt | edgeStmt | comment) <~ "}"
  }
  def nodeStmt: Parser[ParserNode] = id ~ ("[" ~> rep(attr) <~ "]").? ~ ";" ^^ {
    case name ~ None ~ ";"=> 
      ParserNode(name, Map.empty[String, String])
    case name ~ Some(attributes) ~ ";" => 
      ParserNode(name, attributes.foldLeft(Map.empty[String, String]) {
        case (map, elem) => map + elem
      })
    case _ => throw new Exception
  } withFailureMessage("""[ERROR] In parsing node statement; expected one of:
                         | id ";"
                         | id "[" rep(attr) "]" ";"""".stripMargin)
  def edgeStmt: Parser[ParserEdge] = id ~ "->" ~ id ~ ("[" ~> rep(attr) <~ "]").? ~ ";" ^^ {
    case src ~ "->" ~ sink ~ None ~ ";" => 
      ParserEdge(src, sink, Map.empty[String, String])
    case src ~ "->" ~ sink ~ Some(attributes) ~ ";" => 
      ParserEdge(src, sink, attributes.foldLeft(Map.empty[String, String]) {
        case (map, elem) => map + elem
      })
    case _ => throw new Exception
  } withFailureMessage("""[ERROR] In parsing edge statement; expected one of:
                         | id "->" id ";"
                         | id "->" id "[" rep(attr) "]" ";"""".stripMargin)
  def attr: Parser[(String, String)] = id ~ ("=" ~ id).? ^^ { 
    case key ~ None => (key -> "")
    case key ~ Some("=" ~ value) => (key -> value)
    case _ => throw new Exception
  } withFailureMessage("""[ERROR] In parsing attribute; expected one of:
                         | id
                         | id "=" id
                         | id "=" number""".stripMargin)
  def id: Parser[String] = ("""[a-zA-Z_\x80-\xff]+?[a-zA-Z0-9_\x80-\xff]*""".r | """[-]?[0-9]*[.][0-9]+""".r | """[-]?[0-9]+""".r) ^^ { _.toString }
  def comment: Parser[ParserElement] = ("""\/\/[^\n]*""".r | """\/\*[\s\S]*?\*\/""".r) ^^ { 
    x => ParserComment(x.toString)
  }

  /** Return the parsed abstract tree representation from a string */
  def apply(str: String) = parseAll(graph, str) match {
    case Success(result, _) => result
    case failure: NoSuccess => throw new Exception(failure.msg)
  }
}

/** Parser tests */
class DFGSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "DFG parser"

  /** Creates a temporary file, writes the string to it, runs the parser 
   * on it, and deletes the file
   * @param str the path to the DFG file
   */
  def parseStringInFile(str: String)(implicit conf: Parameters): Unit = {
    val file = File.createTempFile("invalid", ".dot")
    (new PrintWriter(file))
      .append(str)
      .close()
    try {
      DFG(file)(conf)
    } finally {
      file.delete()
    }
  }

  /** First, we verify that the main parser fails when given invalid 
   * inputs, and that it successfully parses graphs which are formatted 
   * differently from those produced by the DFG tool but whose structure 
   * should be supported, and that it extracts the right information.
   */

  it should "fail on invalid graph type" in {
    intercept[DFGParseException] {
      parseStringInFile("""graph G {
                          |node[opcode=add];
                          |node->node[operand=0]; // add->add
                          |}
                          |""".stripMargin) }
  }

  it should "fail on syntax errors" in {
    /** Missing semi-colons after node and edge declarations */
    intercept[DFGParseException] {
      parseStringInFile("""digraph G {
                          |node
                          |node->node;
                          |}
                          |""".stripMargin) }

    intercept[DFGParseException] {
      parseStringInFile("""digraph G {
                          |node;
                          |node->node
                          |}
                          |""".stripMargin) }

    /** Invalid (unsupported) edge type */
    intercept[DFGParseException] {
      parseStringInFile("""digraph G {
                          |node;
                          |node--node;
                          |}
                          |""".stripMargin) }

    /** Invalid and multiple attribute lists */
    intercept[DFGParseException] {
      parseStringInFile("""digraph G {
                          |node[opcode=];
                          |node->node;
                          |}
                          |""".stripMargin) }

    intercept[DFGParseException] {
      parseStringInFile("""digraph G {
                          |node[opcode=add][opcode=output];
                          |node->node;
                          |}
                          |""".stripMargin) }
  }

  it should "fail to create edge to/from unknown node" in {
    intercept[DFGMissingNodeException] {
      parseStringInFile("""digraph G {
                          |node1[opcode=output];
                          |node0->node1[operand=0];
                          |}
                          |""".stripMargin) }

    intercept[DFGMissingNodeException] {
      parseStringInFile("""digraph G {
                          |node0[opcode=input];
                          |node0->node1[operand=0];
                          |}
                          |""".stripMargin) }
  }

  it should "fail on missing operand number" in {
    intercept[DFGMissingOperandException] {
      parseStringInFile("""digraph G {
                          |node[opcode=add];
                          |node->node; // add->add
                          |}
                          |""".stripMargin) }
  }

  it should "fail on missing opcode" in {
    intercept[DFGMissingOpcodeException] {
      parseStringInFile("""digraph G {
                          |node;
                          |node->node[operand=0]; // add->add
                          |}
                          |""".stripMargin) }
  }

  it should "fail on missing operands" in {
    intercept[DFGWrongNumberOfOperandsException] {
      parseStringInFile("""digraph G {
                          |const[opcode=const];
                          |add[opcode=add]; // missing operand 1
                          |const->add[operand=0];
                          |}
                          |""".stripMargin) }
  }

  it should "fail on too many operands" in {
    intercept[DFGWrongNumberOfOperandsException] {
      parseStringInFile("""digraph G {
                          |const0[opcode=const];
                          |const1[opcode=const];
                          |const2[opcode=const];
                          |add[opcode=add]; // extra operand 2
                          |const0->add[operand=0];
                          |const1->add[operand=1];
                          |const2->add[operand=2];
                          |}
                          |""".stripMargin) }
  }

  it should "fail on an unspecified opcode" in {
    intercept[DFGMissingOpcodeException] {
      parseStringInFile("""digraph G {
                          |node;
                          |node[opcode=unspecified];
                          |node->node;
                          |}
                          |""".stripMargin) }
  }

  it should "fail on in-ward edge to input node" in {
    intercept[DFGInvalidEdgeException] {
      parseStringInFile("""digraph G {
                          |add[opcode=add];
                          |cst[opcode=const];
                          |cst->add[operand=0];
                          |cst->add[operand=1];
                          |in[opcode=input];
                          |add->in[operand=0];
                          |}
                          |""".stripMargin) }
  }

  it should "fail on out-ward edge from output node" in {
    intercept[DFGInvalidEdgeException] {
      parseStringInFile("""digraph G {
                          |add[opcode=add];
                          |cst[opcode=const];
                          |out[opcode=output];
                          |cst->add[operand=0];
                          |out->add[operand=1];
                          |}
                          |""".stripMargin) }
  }

  it should "support lines beginning with comments" in {
    val file = File.createTempFile("valid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |// I'm a comment
                |node[opcode=add];
                |const[opcode=const];
                |node->node[operand=0]; // add->add
                |const->node[operand=1];
                |}
                |""".stripMargin)
      .close()
    val inlineG = DFG(file)
    inlineG.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("node", "const"))
    inlineG.edges should have size 2


    (new PrintWriter(file))
      .append("""digraph G {
                |/*
                |  I'm a comment
                |*/
                |node[opcode=add];
                |const[opcode=const];
                |node->node[operand=0]; // add->add
                |const->node[operand=1];
                |}
                |""".stripMargin)
      .close()
    val multilineG = DFG(file)
    multilineG.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("node", "const"))
    multilineG.edges should have size 2

    file.delete()
  }

  it should "capture constant values" in {
    import Opcode.OpConst
    val file = File.createTempFile("valid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const value=hello];
                |add[opcode=add];
                |out[opcode=output];
                |in->add[operand=0];
                |cst->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    intercept[DFGInvalidConstantException] { DFG(file) }


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const];
                |add[opcode=add];
                |out[opcode=output];
                |in->add[operand=0];
                |cst->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val smallG = DFG(file)
    val smallConsts = smallG.nodes.filter(_.opcode == OpConst)
    smallConsts should have size 1
    smallConsts.head.attrs("value").toInt should equal (0)


    (new PrintWriter(file))
      .append("""digraph G {
                |cst0[opcode=const value=1234];
                |cst1[opcode=const value=-1234];
                |add[opcode=add];
                |out[opcode=output];
                |cst0->add[operand=0];
                |cst1->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val constG = DFG(file)
    val consts = constG.nodes.filter(_.opcode == OpConst)
    consts should have size 2
    consts.filter(_.name == "cst0").head.attrs("value").toInt should equal ( 1234)
    consts.filter(_.name == "cst1").head.attrs("value").toInt should equal (-1234)

    file.delete()
  }

  it should "update metadata on an already specified node" in {
    val file = File.createTempFile("valid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |node; // one and only node
                |node[opcode=add];
                |node->node[operand=0]; /* add->add */
                |node->node[operand=1];
                |}
                |""".stripMargin)
      .close()
    val graph = DFG(file)
    graph.nodes should have size 1
    graph.nodes.head.name shouldBe "node"
    graph.nodes.head.opcode shouldBe OpAdd
    graph.edges should have size 2


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const];
                |cst[value=hello];
                |add[opcode=add];
                |out[opcode=output];
                |in->add[operand=0];
                |cst->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    intercept[DFGInvalidConstantException] { DFG(file) }


    (new PrintWriter(file))
      .append("""digraph G {
                |in[opcode=input];
                |cst[opcode=const];
                |cst[value=1234];
                |add[opcode=add];
                |out[opcode=output];
                |in->add[operand=0];
                |cst->add[operand=1];
                |add->out[operand=0];
                |}
                |""".stripMargin)
      .close()
    val constG = DFG(file)
    val consts = constG.nodes.filter(_.opcode == OpConst)
    consts should have size 1
    consts.head.attrs("value").toInt should equal (1234)

    file.delete()
  }

  it should "identify fan-in cones" in {
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
    val smallG = DFG(file)
    val in0FI = fanInCone(smallG.nodes.head, smallG)
    in0FI should (have size 1 and contain (smallG.nodes.head))
    val addFI = fanInCone(smallG.nodes.last, smallG)
    addFI should (have size 3 and contain allElementsOf (smallG.nodes))


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
    val mulFI  = fanInCone(largeG.nodes.filter(_.name == "mul").head, largeG)
    mulFI.map(_.name) should (
      have size 5 and
      contain allElementsOf Seq("in0", "in1", "add0", "const", "mul"))

    file.delete()
  }

  it should "fail on non-unique edge" in {
    val file = File.createTempFile("invalid", ".dot")
    (new PrintWriter(file))
      .append("""digraph G {
                |node0[opcode=add];
                |node0->node0[operand=0];
                |node0->node0[operand=0];
                |}
                |""".stripMargin)
      .close()
    intercept[DFGDuplicateDefinitionException] { DFG(file) }
    file.delete()
  }

  it should "check that a graph is a DAG" in {
    // Check that a DAG is identified as a DAG
    val dagFile = File.createTempFile("dag", ".dot")
    (new PrintWriter(dagFile))
      .append("""digraph G {
                |const0[opcode=const];
                |const1[opcode=const];
                |add0[opcode=add];
                |const0->add0[operand=0];
                |const1->add0[operand=1];
                |}
                |""".stripMargin)
      .close()
    isDAG(DFG(dagFile)) shouldBe true
    dagFile.delete()


    // Check that a graph with a cycle is identified as not being a DAG
    val nonDagFile = File.createTempFile("non_dag", ".dot")
    (new PrintWriter(nonDagFile))
      .append("""digraph G {
                |mul0[opcode=mul];
                |add1[opcode=add];
                |output2[opcode=output];
                |mul0->add1[operand=0];
                |add1->add1[operand=1];
                |add1->mul0[operand=0];
                |mul0->mul0[operand=1];
                |add1->output2[operand=0];
                |}
                |""".stripMargin)
      .close()
    isDAG(DFG(nonDagFile)) shouldBe false
    nonDagFile.delete()
  }

  it should "check that a graph is connected" in {
    // Check that a connected graph is identified as connected
    val conFile = File.createTempFile("connected", ".dot")
    (new PrintWriter(conFile))
      .append("""digraph G {
                |const0[opcode=const];
                |const1[opcode=const];
                |add0[opcode=add];
                |const0->add0[operand=0];
                |const1->add0[operand=1];
                |}
                |""".stripMargin)
      .close()
    isConnected(DFG(conFile)) shouldBe true
    conFile.delete()


    // Check that a disconnected graph is identified as not connected
    val disconFile = File.createTempFile("disconnected", ".dot")
    (new PrintWriter(disconFile))
      .append("""digraph G {
                |const0[opcode=const];
                |const1[opcode=const]; // disconnected
                |add0[opcode=add];
                |const0->add0[operand=0];
                |const0->add0[operand=1];
                |}
                |""".stripMargin)
      .close()
    isConnected(DFG(disconFile)) shouldBe false
    disconFile.delete()
  }

  it should "produce a collection of connected components" in {
    val file = File.createTempFile("valid", ".dot")
    // Check that an empty graph is identified as empty
    (new PrintWriter(file))
      .append("""digraph G {}""")
      .close()
    components(DFG(file)) shouldBe empty


    // Check that a connected graph is returned alone
    (new PrintWriter(file))
      .append("""digraph G {
                |const0[opcode=const];
                |const1[opcode=const];
                |add0[opcode=add];
                |const0->add0[operand=0];
                |const1->add0[operand=1];
                |}
                |""".stripMargin)
      .close()
    components(DFG(file)) should have size 1


    // Check that a disconnected graph is split into its components
    (new PrintWriter(file))
      .append("""digraph G {
                |const0[opcode=const];
                |const1[opcode=const]; // disconnected
                |add0[opcode=add];
                |const0->add0[operand=0];
                |const0->add0[operand=1];
                |}
                |""".stripMargin)
      .close()
    val cmpnts = components(DFG(file))
      .sortBy(_.nodes.size)
    cmpnts should have size 2
    cmpnts.head.nodes.map(_.name) should (
      have size 1 and
      contain ("const1"))
    cmpnts.last.nodes.map(_.name) should (
      have size 2 and
      contain allElementsOf Seq("const0", "add0"))

    file.delete()
  }

  it should "produce a topological sorting of a simple DAG" in {
    val dagFile = File.createTempFile("dag", ".dot")
    (new PrintWriter(dagFile))
      .append("""digraph G {
                |const0[opcode=const];
                |const1[opcode=const];
                |add0[opcode=add];
                |const0->add0[operand=0];
                |const1->add0[operand=1];
                |}
                |""".stripMargin)
      .close()
    topologicalSort(DFG(dagFile)).last.name should be ("add0")
    dagFile.delete()
  }

  it should "produce topological sortings" in {
    // Get all DFG files from the resource directory
    val dfgFiles = (new File("src/test/resources/dfgparse"))
      .listFiles().filter(_.isFile).map(_.getAbsolutePath()).toList

    // Check the topological sortings
    dfgFiles.foreach { file =>
      val dfg = DFG(file)
      val srtng = topologicalSort(dfg)

      srtng should contain theSameElementsAs (dfg.nodes)
    }
  }

  /** This test is meant to verify that the main DFG parser object provides 
   * the correct graph as output. It does so by parsing a DFG, converting 
   * it to the internal format, outputting it to the DOT-format, and 
   * checking the two's equivalence using a simplified parser defined here.
   * 
   * Two graphs are equivalent iff they have the same vertices and the same 
   * edges. We check this manually. Using the same reference parser for 
   * the original and the parsed graphs isolates errors in the reference.
   */

  it should "produce equivalent graphs" in {
    // Get all DFG files from the resource directory
    val dfgFiles = (new File("src/test/resources/dfgparse"))
      .listFiles.filter(_.isFile).map(_.getAbsolutePath()).toList

    // Check their equivalence through the reference parser
    dfgFiles.foreach { file =>
      val dfg = reference(DFG(file).toString())
      val ref = reference(scala.io.Source.fromFile(file).getLines().mkString("\n"))

      // Check the vertex sets (ignore attribute mismatches introduced by 
      // automatic inference of constants in the parser)
      val dfgNodes = dfg.collect { case x: ParserNode => x }
      val refNodes = ref.collect { case x: ParserNode => x }
        .map { node =>
          if (node.attrs("opcode") == "const" && !node.attrs.contains("value"))
            new ParserNode(node.name, node.attrs + ("value" -> "0"))
          else node }
      dfgNodes.map(_.name) should contain theSameElementsAs (refNodes.map(_.name))
      dfgNodes should contain theSameElementsAs (refNodes)

      // Check the edges
      val dfgEdges = dfg.collect { case x: ParserEdge => x }
      val refEdges = ref.collect { case x: ParserEdge => x }
      dfgEdges should contain theSameElementsAs (refEdges)

      // Check vertex names in edges
      val dfgNodeNames = dfgNodes.map(_.name).toSet
      dfgEdges.foreach { edge => dfgNodeNames should (contain (edge.src) and contain (edge.sink)) }
    }
  }
}
