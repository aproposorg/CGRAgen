package cgragen.dfgparse

import cgragen.dfgparse.Opcode.{Opcode, OpConst, OpInput, OpOutput, OpUnspecified}

import scala.collection.mutable

import scala.util.parsing.combinator.RegexParsers
import scala.util.Try

/** Data flow graph comprising operation nodes and data dependency edges
 * @param name the name of the graph (defaults to "G")
 */
private[cgragen] final class DFG(val name: String = "G") {
  /** Fields start ***********************************************************/
  private val _nodes = mutable.ArrayBuffer.empty[DFGNode]
  private val _edges = mutable.ArrayBuffer.empty[DFGEdge]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the nodes of this DFG */
  def nodes: Array[DFGNode] = _nodes.toArray

  /** Return the edges of this DFG */
  def edges: Array[DFGEdge] = _edges.toArray
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a node to this DFG
   * @param node the node to add
   */
  def addNode(node: DFGNode): Unit = {
    require(!_nodes.exists(_ == node))
    _nodes += node
  }

  /** Remove a node from this DFG
   * @param node the node to remove
   */
  def removeNode(node: DFGNode): Unit = _nodes -= node

  /** Add an edge to this DFG
   * @param edge the edge to add
   */
  def addEdge(edge: DFGEdge): Unit = {
    require(!_edges.exists(_ == edge))
    _edges += edge
  }

  /** Remove an edge from this DFG
   * @param edge the edge to remove
   */
  def removeEdge(edge: DFGEdge): Unit = _edges -= edge
  /** Modifiers end **********************************************************/

  /** Various other methods start ********************************************/
  /** Return the DFG in a string */
  override def toString(): String = {
    val bs = new StringBuilder(s"digraph $name {\n")

    // Add all nodes to the string
    _nodes.foreach { node => bs ++= s"${node};\n" }

    // Add all edges to the string
    _edges.foreach { edge => bs ++= s"${edge};\n" }

    // Finalize and return the string
    bs ++= "}\n"
    bs.mkString
  }
  /** Varioud other methods end **********************************************/
}

private[cgragen] object DFG extends RegexParsers {
  /** Abstract elements to parse into and to simplify subsequent matching */
  private[dfgparse] trait ParserElement
  private[dfgparse] case class ParserNode(name: String, attrs: Map[String, String])
    extends ParserElement
  private[dfgparse] case class ParserEdge(src: String, sink: String, attrs: Map[String, String])
    extends ParserElement
  private[dfgparse] case class ParserComment(msg: String)
    extends ParserElement

  /** Define the grammar of the sub-set of the DOT language used here. Includes the 
   * following terms:
   * - graph
   * - nodeStmt
   * - edgeStmt
   * - attr
   * - id
   * - comment
   * Attributes are key-value pairs, both interpreted as strings. Comments are ignored.
   */
  private[DFG] def graph: Parser[List[ParserElement]] = {
    "digraph" ~ id.? ~ "{" ~> rep(nodeStmt | edgeStmt | comment) <~ "}"
  }
  private[DFG] def nodeStmt: Parser[ParserNode] = id ~ ("[" ~> rep(attr) <~ "]").? ~ ";" ^^ {
    // "nodename;"
    case name ~ None ~ ";" => 
      ParserNode(name, Map.empty[String, String])
    // "nodename[attrs];"
    case name ~ Some(attributes) ~ ";" => 
      ParserNode(name, attributes.foldLeft(Map.empty[String, String]) {
        case (map, elem) => map + elem
      })
    // should never be reached
    case _ => throw new Exception
  } withFailureMessage("""[ERROR] In parsing node statement; expected one of:
                         | id ";"
                         | id "[" rep(attr) "]" ";"""".stripMargin)
  private[DFG] def edgeStmt: Parser[ParserEdge] = id ~ "->" ~ id ~ ("[" ~> rep(attr) <~ "]").? ~ ";" ^^ {
    // "src->sink;"
    case src ~ "->" ~ sink ~ None ~ ";" => 
      ParserEdge(src, sink, Map.empty[String, String])
    case src ~ "->" ~ sink ~ Some(attributes) ~ ";" => 
      ParserEdge(src, sink, attributes.foldLeft(Map.empty[String, String]) {
        case (map, elem) => map + elem
      })
    // should never be reached
    case _ => throw new Exception
  } withFailureMessage("""[ERROR] In parsing edge statement; expected one of:
                         | id "->" id ";"
                         | id "->" id "[" rep(attr) "]" ";"""".stripMargin)
  private[DFG] def attr: Parser[(String, String)] = id ~ ("=" ~ id).? ^^ { 
    // "key"
    case key ~ None => (key -> "")
    // "key=value"
    case key ~ Some("=" ~ value) => (key -> value)
    // should never be reached
    case _ => throw new Exception
  } withFailureMessage("""[ERROR] In parsing attribute; expected one of:
                         | id
                         | id "=" id
                         | id "=" number""".stripMargin)
  private[DFG] def id: Parser[String] = ("""[a-zA-Z_\x80-\xff]+?[a-zA-Z0-9_\x80-\xff]*""".r | """[-]?[0-9]*[.][0-9]+""".r | """[-]?[0-9]+""".r) ^^ {
    _.toString
  }
  private[DFG] def comment: Parser[ParserElement] = ("""\/\/[^\n]*""".r | """\/\*[\s\S]*?\*\/""".r) ^^ { 
    x => ParserComment(x.toString)
  }

  /** Parse a DOT-formatted DFG file
   * @param file a string containing an DFG file path
   * @return a new instance of [[cgragen.dfgparse.DFG]] resulting from the parsing
   */
  def apply(file: String)(implicit conf: cgragen.Parameters): DFG = {
    if (conf.DFGDebug)
      println(s"[DEBUG] Begin parsing DFG from file ($file)")
    
    // Load the input file as a string and parse it
    val input  = scala.io.Source.fromFile(file).getLines().mkString("\n")
    val parsed = parseAll(graph, input) match {
      case Success(res, _) => res
      case flr: NoSuccess  =>
        println(s"[ERROR] Failed to parse DFG in file ($file) with errors:\n${flr.msg}")
        throw new DFGParseException(flr.msg)
    }

    // Create an empty DFG and add the parsed nodes and edges to it
    val dfg = new DFG()

    // ... start with the nodes
    val incompleteNodes = mutable.HashMap.empty[String, mutable.HashMap[String, String]]
    parsed.collect { case ParserNode(name, attrs) =>
      // Get the node's opcode, if any
      val opcode = Try(Opcode.withName(attrs("opcode"))).getOrElse(OpUnspecified)

      // Check if the node already exists
      dfg.nodes.collectFirst { case node if node.name == name => node } match {
        case Some(existing) => // a node with the same name already exists
          if (opcode != OpUnspecified && existing.opcode != opcode) {
            // The node definition does not match the existing one
            print(s"[ERROR] Found duplicate node definition ($name) with ")
            println(s"mismatching opcodes ($opcode != ${existing.opcode})")
            throw new DFGDuplicateDefinitionException("mismatching opcodes")
          } else {
            // The node definition matches the existing one
            attrs.foreach { case (key, value) => existing.addAttr(key, value) }

            if (conf.DFGDebug)
              println(s"[DEBUG] Updated attributes in duplicated DFG node ($name)")
          }

        case _ => // a node with the same name does not exist
          if (opcode != OpUnspecified) {
            // Create a new node with the given information
            val node = new DFGNode(name, opcode)
            val incmpltAttrs = incompleteNodes.getOrElse(name, mutable.HashMap.empty[String, String])
            (incmpltAttrs ++ attrs).foreach { case (key, value) => node.addAttr(key, value) }

            // Add the node to the DFG and mark it as complete
            dfg.addNode(node)
            incompleteNodes -= name

            if (conf.DFGDebug) println(s"[DEBUG] Added new DFG node ($name)")

          } else {
            // Mark the node as incomplete
            if (incompleteNodes.contains(name)) incompleteNodes(name) ++= attrs
            else incompleteNodes(name) = mutable.HashMap.from(attrs)

            if (conf.DFGDebug) println(s"[DEBUG] Added incomplete DFG node ($name)")
          }
      }
    }

    // ... verify that no incomplete nodes are left
    if (incompleteNodes.nonEmpty) {
      print("[ERROR] Cannot map DFG with nodes with unspecified opcodes - ")
      println(s"found (${incompleteNodes.mkString("[", " ", "]")})")
      throw new DFGMissingOpcodeException("unspecified opcode")
    }

    // ... proceed with the edges
    parsed.collect { case ParserEdge(srcName, snkName, attrs) =>
      // Ensure the source and sink nodes already exist and that the operand 
      // index is valid
      if (!dfg.nodes.exists(_.name == srcName)) {
        println(s"[ERROR] Edge has non-declared source node ($srcName)")
        throw new DFGMissingNodeException("source node missing")
      }
      if (!dfg.nodes.exists(_.name == snkName)) {
        println(s"[ERROR] Edge has non-declared sink node ($snkName)")
        throw new DFGMissingNodeException("sink node missing")
      }
      if (!attrs.contains("operand") || attrs("operand").toIntOption == None) {
        println(s"[ERROR] Edge ($srcName)->($snkName) is missing an operand number")
        throw new DFGMissingOperandException("operand number missing")
      }

      // Get the source and sink nodes and the operand index
      val src = dfg.nodes.collectFirst { case node if node.name == srcName => node }.get
      val snk = dfg.nodes.collectFirst { case node if node.name == snkName => node }.get
      val operand = attrs("operand").toInt

      // Check that the source is not an output node
      if (src.opcode == OpOutput) {
        println(s"[ERROR] Cannot add outward edge from output node ($srcName)->($snkName)")
        throw new DFGInvalidEdgeException("invalid source")
      }

      // Check that the sink is not an input node
      if (snk.opcode == OpInput) {
        println(s"[ERROR] Cannot add inward edge to input node ($srcName)->($snkName)")
        throw new DFGInvalidEdgeException("invalid sink")
      }

      // Create a new edge with the given information
      val edge = new DFGEdge(src, snk, operand)
      if (dfg.edges.exists(_ == edge)) {
        println(s"[ERROR] Found duplicate edge definition ($srcName)->($snkName)")
        throw new DFGDuplicateDefinitionException("duplicate edge")
      } else {
        dfg.addEdge(edge)

        if (conf.DFGDebug) println(s"[DEBUG] Added new DFG edge ($srcName)->($snkName)")
      }
    }

    // Verify or set default values in constant nodes
    dfg.nodes
      .filter(_.opcode == OpConst)
      .foreach { const =>
      const.attrs.get("value") match {
        case Some(value) if value.toIntOption == None =>
          // Invalid value passed, throw an error
          println(s"[ERROR] Invalid constant value ($value) in node (${const.name})")
          throw new DFGInvalidConstantException("invalid constant")

        case None =>
          // No value passed, set the constant to a default value of 0
          const.addAttr("value", "0")

          if (conf.DFGDebug)
            println(s"[DEBUG] Set default constant value (0) in node (${const.name})")

        case _ =>
          // no action needed
      }}

    if (conf.DFGDebug)
      println(s"[DEBUG] Finished parsing DFG (${dfg.name}) from file ($file)")

    // Check for missing or too many operands
    val missOprs = dfg.nodes
      .filter { node =>
        val req = reqOperands(node.opcode)
        val ins = dfg.edges.filter(_.snk == node)
        ins.size < req || !(0 until req).forall(i => ins.map(_.operand).contains(i))
      }
    val manyOprs = dfg.nodes
      .filter { node =>
        dfg.edges.filter(_.snk == node).size > reqOperands(node.opcode) }
    if (missOprs.nonEmpty) {
      print("[ERROR] Cannot map DFG with nodes missing operands - ")
      println(s"found (${missOprs.mkString("[", ", ", "]")})")
      throw new DFGWrongNumberOfOperandsException("missing operands")
    } else if (manyOprs.nonEmpty) {
      print("[ERROR] Cannot map DFG with nodes with too many operands - ")
      println(s"found (${manyOprs.mkString("[", ", ", "]")})")
      throw new DFGWrongNumberOfOperandsException("too many operands")
    }

    dfg
  }

  /** Check whether a DFG is connected
   * @param dfg the DFG to check
   * @return true iff `dfg` is a connected graph
   */
  def isConnected(dfg: DFG): Boolean = dfg.nodes.isEmpty || {
    // Keep track of visited nodes
    val visited = mutable.HashSet.empty[DFGNode]

    // Perform a DFS assuming edges are undirected
    def _dfs(node: DFGNode): Unit = {
      visited += node
      // Run through all out-going edges
      dfg.edges
        .filter (edge => edge.src == node && !visited(edge.snk))
        .foreach(edge => _dfs(edge.snk))

      // Run through all in-going edges
      dfg.edges
        .filter (edge => edge.snk == node && !visited(edge.src))
        .foreach(edge => _dfs(edge.src))
    }
    _dfs(dfg.nodes.head)

    // The DFG is connected if all nodes have been visited
    dfg.nodes.forall(visited(_))
  }

  /** Compute the connected components of a DFG
   * @param dfg the DFG to compute the components of
   * @return a list of the components in `dfg`
   * 
   * @note If `dfg` is empty, so is the returned list. If `dfg` is connected 
   *       and non-empty, the returned list contains only `dfg` itself. 
   *       Otherwise, the list includes all non-empty components of `dfg`.
   */
  def components(dfg: DFG): Seq[DFG] = dfg match {
    case empty if empty.nodes.isEmpty => Seq.empty[DFG]
    case conn  if isConnected(conn)   => Seq(conn)
    case _ =>
      // Keep track of visited nodes and components
      val cmpnts  = mutable.ArrayBuffer.empty[DFG]
      val visited = mutable.HashSet.empty[DFGNode]

      // Perform a DFS assuming edges are undirected
      def _dfs(node: DFGNode, vstd: Set[DFGNode] = Set.empty[DFGNode]): Seq[DFGNode] = {
        // Add this node to the set of visited nodes
        val vstdWNode = vstd + node

        // Run through all out-going edges
        val vstdWOuts = dfg.edges
          .filter(edge => edge.src == node && !vstdWNode(edge.snk)) // conservative filter
          .foldLeft(vstdWNode) { case (acc, edge) => acc ++ _dfs(edge.snk, acc) }

        // Run through all in-going edges
        val vstdWIns = dfg.edges
          .filter(edge => edge.snk == node && !vstdWOuts(edge.src)) // conservative filter
          .foldLeft(vstdWOuts) { case (acc, edge) => acc ++ _dfs(edge.src, acc) }

        // Return all the visited nodes
        vstdWIns.toSeq
      }

      // Build the components one by one
      while (dfg.nodes.size != visited.size) {
        // Pick the first non-visited node
        val head = dfg.nodes
          .collectFirst { case node if !visited(node) => node }
          .get // guaranteed by while condition

        // Build a sub-DFG with nodes and edges for this component
        val subdfg = new DFG(s"${dfg.name}#${cmpnts.size}")
        val nodes  = _dfs(head)
        nodes.foreach { node =>
          subdfg.addNode(node)
          dfg.edges
            .filter(_.src == node)
            .foreach(subdfg.addEdge(_))
          visited += node
        }

        // Add the sub-DFG to the list of components
        cmpnts += subdfg
      }

      cmpnts.toSeq
  }

  /** Check whether a DFG is a DAG
   * @param dfg the DFG to check
   * @return true iff `dfg` is a directed acyclic graph
   */
  def isDAG(dfg: DFG): Boolean = dfg.nodes.isEmpty || {
    // Perform a DFS starting in nodes with only out-going edges
    def _hasBackwardEdge(node: DFGNode, visited: Set[DFGNode] = Set.empty[DFGNode]): Boolean = {
      visited(node) ||
        dfg.edges
          .filter(_.src == node)
          .map(edge => _hasBackwardEdge(edge.snk, visited + node))
          .foldLeft(false){ case (acc, elem) => acc || elem }
    }

    // The DFG is a DAG if no DFS starting from a node with only out-going 
    // edges detects a backward edge
    val snks = dfg.edges.map(_.snk).toSet
    dfg.nodes.exists(!snks(_)) &&
      dfg.nodes
        .filter(!snks(_))
        .foldLeft(true) { case (acc, node) => acc && !_hasBackwardEdge(node) }
  }

  /** Topologically sort the nodes in a DFG
   * @param dfg the DFG whose nodes to sort
   * @return a topologically sorted list of the nodes in `dfg`
   * 
   * @note DFGs that are not DAGs cannot be topologically sorted.
   * 
   * @todo fix this? Seems broken.
   */
  def topologicalSort(dfg: DFG): Seq[DFGNode] = {
    require(isDAG(dfg), "cannot topologically sort DFGs with loops")
    // Keep track of visited nodes and their topological order
    val visited = mutable.HashSet.empty[DFGNode]
    val order   = mutable.ArrayBuffer.empty[DFGNode]

    // Perform a BFS starting in nodes with only out-going edges
    val snks = dfg.edges.map(_.snk).toSet
    val queue = mutable.ArrayBuffer(dfg.nodes.filterNot(snks(_)).toSeq:_*)
    while (queue.nonEmpty) {
      val node = queue.head
      queue.dropInPlace(1)
      val nghbrs = dfg.edges
        .filter(edge => edge.src == node && !visited(edge.snk))
        .map(_.snk)
      visited ++= nghbrs
      queue   ++= nghbrs
      order += node
    }

    order.toSeq
  }

  /** Return the fan-in cone of a given starting node in a DFG
   * @param start the node from which to start the search
   * @param dfg the DFG to search through
   * @return a list of nodes in the fan-in cone of `start` in `dfg`
   * 
   * @note Returns an empty list if `start` is not in `dfg`.
   */
  def fanInCone(start: DFGNode, dfg: DFG): Seq[DFGNode] = {
    val visited = mutable.HashSet.empty[DFGNode]

    // Recursively get the fan-in cone of a node
    def _fanin(head: DFGNode): Seq[DFGNode] = {
      visited += head
      val prnts = dfg.edges
        .filter(_.snk == head)
        .flatMap {
          case edge if !visited(edge.src) => _fanin(edge.src)
          case _ => Seq.empty[DFGNode] }
      Seq(head) ++ prnts
    }

    if (dfg.nodes.contains(start)) _fanin(start) else Seq.empty[DFGNode]
  }
}
