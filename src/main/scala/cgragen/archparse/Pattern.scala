package cgragen.archparse

import scala.collection.mutable

import scala.xml.Node

/** Abstract pattern
 * @param startRow the start row index of the pattern
 * @param endRow the end row index of the pattern
 * @param startCol the start column index of the pattern
 * @param endCol the end column index of the pattern
 * @param rowInc the row index increment
 * @param colInc the column index increment
 * @param doWrapCol whether the pattern wraps its column indices
 * @param doWrapRow whether the pattern wraps its row indices
 */
private[archparse] final class Pattern(
  val startRow : Int, 
  val endRow   : Int, 
  val startCol : Int, 
  val endCol   : Int, 
  val rowInc   : Int, 
  val colInc   : Int, 
  val doWrapCol: Boolean,
  val doWrapRow: Boolean
) {
  /** Fields start ***********************************************************/
  // Abstract representation of connections in this pattern with 
  // to-connection types being one of "to" or "distribute-to"
  // and from-connection types being "from"
  private val _connections = mutable.ArrayBuffer.empty[Connection]

  // Abstract representation of sub-modules in this pattern
  private val _subModules = mutable.HashMap.empty[String, String]
  /** Fields end *************************************************************/

  /** Accessors start ********************************************************/
  /** Return the connections of this pattern */
  def connections = _connections.toArray

  /** Return the sub-modules of this pattern */
  def subModules = _subModules.toMap
  /** Accessors end **********************************************************/

  /** Modifiers start ********************************************************/
  /** Add a connection to this pattern
   * @param conn the connection to add
   */
  def addConnection(conn: Connection): Unit = _connections += conn

  /** Add a sub-module instance to this pattern
   * @param modName the name of the sub-module
   * @param modType the type of the sub-module
   */
  def addSubModule(modName: String, modType: String): Unit = _subModules(modName) = modType
  /** Modifiers end **********************************************************/
}

private[archparse] object Pattern {
  /** Matches arguments in the form of "(rel a b).portname" or "modulename.portname" */
  private val relRegex = """((\(rel\s*[+-]?[0-9]+\s*[+-]?[0-9]+\))|([a-zA-Z][a-zA-Z0-9_()]*))\.([a-zA-Z][a-zA-Z0-9_()]*)""".r

  /** Matches arguments in the form of "a + b" */
  private val sumRegex = """([0-9]+\s*)\+(\s*[0-9]+)""".r

  /** Matches arguments in the form of "a * b" */
  private val prodRegex = """([0-9]+\s*)\*(\s*[0-9]+)""".r

  /** Parse an XML description of a pattern
   * @param pat the XML description of the pattern
   * @param defs a map of constant definitions in the architecture
   * @return a new instance of [[cgragen.archparse.Pattern]] resulting from 
   *         the parsing
   */
  def apply(pat: Node, defs: Map[String, Int])(implicit conf: cgragen.Parameters) = {
    // First get the row and column indices
    if (conf.CGRADebug) println(s"[DEBUG] Scanning for row- and column-ranges in pattern")
    val (startRow, endRow) = _extractIndices(pat, "row-range", defs)
    val (startCol, endCol) = _extractIndices(pat, "col-range", defs)
    if (conf.CGRADebug) {
      print(s"[DEBUG] Pattern has row-range [$startRow:$endRow] and ")
      println(s"column-range [$startCol:$endCol]")
    }

    // Next, get the row and column increments and skips
    if (conf.CGRADebug) println(s"[DEBUG] Scanning for increments and skips in pattern")
    val (rowInc, colInc) = {
      val (rows, cols) = { // defaults to (1, 1)
        def getOrOne(c: String) = {
          val key = (pat \@ c)
          key.toIntOption match {
            case Some(value) => value
            case _ => if (defs.contains(key)) defs(key) else 1
          }
        }
        ((pat \ "@rows").isEmpty, (pat \ "@cols").isEmpty) match {
          case (true,  true)  => (getOrOne("rows"), getOrOne("cols"))
          case (true,  false) => (getOrOne("rows"), 1)
          case (false, true)  => (1, getOrOne("cols"))
          case (false, false) => (1, 1)
        }
      }
      val (rowSkip, colSkip) = { // defaults to (0, 0)
        def getOrZero(c: String) = {
          val key = (pat \@ c)
          key.toIntOption match {
            case Some(value) => value
            case _ => if (defs.contains(key)) defs(key) else 0
          }
        }
        ((pat \ "@row-skip").isEmpty, (pat \ "@col-skip").isEmpty) match {
          case (true,  true)  => (getOrZero("row-skip"), getOrZero("col-skip"))
          case (true,  false) => (getOrZero("row-skip"), 0)
          case (false, true)  => (0, getOrZero("col-skip"))
          case (false, false) => (0, 0)
        }
      }
      (rows + rowSkip, cols + colSkip)
    }
    if (conf.CGRADebug) println(s"[DEBUG] Pattern has row-increment ($rowInc) and column-increment ($colInc)")

    // Determine if the pattern should wrap on columns and rows
    val doWrapAround = (pat \ "@wrap-around").nonEmpty && ((pat \@ "wrap-around").toIntOption match {
      case Some(value) => value != 0
      case _ =>
        println(s"[ERROR] Pattern has invalid wrap-around attribute")
        throw new MissingDataException("invalid wrap-around")
    })
    val doWrapCol = doWrapAround || {
      (pat \ "@wrap-col").nonEmpty && ((pat \@ "wrap-col").toIntOption match {
        case Some(value) => value != 0
        case _ =>
          println(s"[ERROR] Pattern has invalid wrap-col attribute")
          throw new MissingDataException("invalid wrap-col")
      })
    }
    val doWrapRow = doWrapAround || {
      (pat \ "@wrap-row").nonEmpty && ((pat \@ "wrap-row").toIntOption match {
        case Some(value) => value != 0
        case _ =>
          println(s"[ERROR] Pattern has invalid wrap-row attribute")
          throw new MissingDataException("invalid wrap-row")
      })
    }

    // Next, get counter names and whether the pattern should wrap
    if (conf.CGRADebug) println("[DEBUG] Scanning for counters in pattern")
    val counterAttrs = Seq("counter", "row-counter", "col-counter")
    val counters     = counterAttrs.foldLeft(Map.empty[String, String]) { case (acc, attr) =>
      if ((pat \ s"@$attr").nonEmpty) {
        val cntrName = pat \@ attr
        // Verify that the counter name is unique
        if (acc.values.toSeq.contains(cntrName)) {
          println(s"[ERROR] Pattern has $attr with non-unique name")
          throw new DuplicateDefinitionException("non-unique counter name")
        }
        acc + (attr -> cntrName)
      } else {
        acc
      }
    }
    // Verify that no counter has the same name as a constant
    counters.values.filter(cntrName => defs.contains(cntrName)).foreach { cntrName =>
      println(s"[ERROR] Pattern has counter with the same name as a constant")
      throw new DuplicateDefinitionException("non-unique counter name")
    }
    if (conf.CGRADebug) {
      println(s"[DEBUG] Pattern has counters (${counters.map { case (attr, cntrName) =>
        s"$attr=$cntrName"
      }.mkString(", ")})")
      println(s"[DEBUG] Pattern does ${(doWrapRow, doWrapCol) match {
        case (true, true)   => "wrap around on rows and columns"
        case (true, false)  => "wrap around on rows"
        case (false, true)  => "wrap around on columns"
        case (false, false) => "not do wrap around"
      }}")
    }

    // Verify that there are not too many blocks defined in the pattern
    val cellCount = rowInc * colInc
    if ((pat \ "block").length > cellCount) {
      println(s"[ERROR] Pattern has too many modules per cell")
      throw new DuplicateFieldException("too many blocks in pattern")
    }

    // Next, expand the pattern by running through all rows and columns
    if (conf.CGRADebug) println(s"[DEBUG] Expanding pattern contents")
    val pattern = new Pattern(startRow, endRow, startCol, endCol, rowInc, colInc, doWrapCol, doWrapRow)
    (startRow to endRow by rowInc).foreach { row =>
      (startCol to endCol by colInc).foreach { col =>
        // Implement counter values for simpler re-use
        val counterVals = Map(
          "counter"     -> ((row - startRow) * (endCol - startCol + 1) + (col - startCol)), 
          "row-counter" -> (row - startRow), 
          "col-counter" -> (col - startCol)
        )

        // Next, add blocks as sub-modules to the pattern
        if (conf.CGRADebug) println(s"[DEBUG] Scanning for sub-module instances in pattern")
        _addSubModules(pat, row, col, pattern)

        /** Given an identifier with potential counter names in it, replace 
         * the names by their respective values
         * @param idtf the string identifier
         * @return the expanded version of the identifier
         */
        def expandCounters(idtf: String) = counters.foldLeft(idtf) { case (acc, (attr, cntrName)) =>
          acc.replaceAll(s"\\($cntrName\\)", counterVals(attr).toString)
        }

        /** Given an identifier with potential constant names in it, replace 
         * the names by their respective values
         * @param idtf the string identifier
         * @return the expanded version of the identifier
         */
        def expandDefinitions(idtf: String) = defs.foldLeft(idtf) { case (acc, (defName, defValue)) =>
          acc.replaceAll(s"\\($defName\\)", defValue.toString)
        }

        // Next, add connections to the pattern
        if (conf.CGRADebug) println(s"[DEBUG] Scanning for connections in pattern")
        (pat \ "connection").foreach { conn =>
          // Connections must have from- and to-types, and the types must be 
          // (from, to), with corresponding port name arguments
          if ((conn \ "@from").isEmpty && (conn \ "@select-from").isEmpty) {
            println(s"[ERROR] Pattern has connection ($conn) with no from-type")
            throw new MissingDataException("missing from-type")
          } else if (!(conn \ "@from").isEmpty && !(conn \ "@select-from").isEmpty) {
            println(s"[ERROR] Pattern has connection ($conn) with multiple from-types")
            throw new DuplicateDataException("multiple from-types")
          }
          if ((conn \ "@to").isEmpty && (conn \ "@distribute-to").isEmpty) {
            println(s"[ERROR] Pattern has connection ($conn) with no to-type")
            throw new MissingDataException("missing to-type")
          } else if (!(conn \ "@to").isEmpty && !(conn \ "@distribute-to").isEmpty) {
            println(s"[ERROR] Pattern has connection ($conn) with multiple to-types")
            throw new DuplicateDataException("multiple to-types")
          }
          val fromType = if ((conn \ "@from").isEmpty) "select-from" else "from"
          val fromArgs = conn \@ fromType
          val toType   = if ((conn \ "@to").isEmpty) "distribute-to" else "to"
          val toArgs   = conn \@ toType

          // Verify that none of the arguments are empty
          if (fromArgs == "") {
            println(s"[ERROR] Pattern has connection with no from-arguments")
            throw new MissingDataException("missing from-arguments")
          }
          if (toArgs == "") {
            println(s"[ERROR] Pattern has connection with no to-arguments")
            throw new MissingDataException("missing to-arguments")
          }

          // Connections may have an optional sign-extension parameter passed to them
          val sext = {
            val sextOpt = if ((conn \ "@sext").isEmpty) None else Some(conn \@ "sext")
            sextOpt match {
              case Some(arg) =>
                arg.toIntOption match {
                  case Some(num) => num != 0
                  case _ =>
                    println(s"[ERROR] Pattern has connection with invalid sign-extension argument")
                    throw new MissingDataException("invalid sign-extension argument")
                }
              case _ => conf.Sext
            }
          }

          /* Arguments can have relative operators in them - these must be expanded for 
           * the names to make sense. CGRA-ME does so through a number of re-writings, 
           * but we do it locally to avoid passing around too many variables. Also, do
           * counter arithmetic to determine arguments without counter names.
           */

          // Expand counter and definition values in from- and to-arguments
          /** Given a string of connection arguments, expand their counter and 
           * constant values, and split them into an array
           * @param args the string of connection arguments
           * @return an array of expanded arguments
           */
          def expandArguments(args: String) = {
            val expanded = expandDefinitions(expandCounters(args))
            // Verify that no counter or constant names are left
            "\\(([a-zA-Z]+)\\)".r.findAllIn(expanded).foreach { idtf =>
              println(s"[ERROR] Pattern has connection with undeclared counter or constant ($idtf)")
              throw new MissingDataException("undeclared counter or constant")
            }
            relRegex.findAllIn(expanded)
          }
          val expFromArgs = expandArguments(fromArgs)
          val expToArgs   = expandArguments(toArgs)

          // Generate the proper block and port identifiers by collapsing 
          // relative ones
          /** Fully collapse a proper or relative identifier
           * @param idtf the identifier to collapse
           * @return the collapsed version of the identifier
           */
          def collapseIdentifier(idtf: String) = _collapseMath(
            if (idtf.startsWith("(rel")) _collapseRelative(idtf, row, col, pattern) else idtf
          )
          val properFromArgs = expFromArgs.map(collapseIdentifier(_)).toArray
          val properToArgs = expToArgs.map(collapseIdentifier(_)).toArray

          // The only valid combination of from- and to-types is (from, to)
          (fromType, toType) match {
            case ("from", "to") =>
              // Verify that there is only one argument to from- and to-connections
              if (properFromArgs.length != 1 || properToArgs.length != 1) {
                println("[ERROR] Pattern has (from, to) connection with multiple from- or to-connections")
                throw new DuplicateDataException("multiple from- or to-arguments")
              }
              // Should not connect to a wire, simply add as a new connection
              pattern.addConnection(Connection(toType, properToArgs.head, fromType, properFromArgs.head, sext))
              if (conf.CGRADebug) {
                println(s"[DEBUG] Added connection from (${properFromArgs.head}) to (${properToArgs.head})")
              }

            case _ =>
              // If no previous case matched, the port type combination is invalid
              println(s"[ERROR] Pattern has connection ($conn) with invalid (from, to) type pair")
              throw new MissingDataException("invalid (from, to) type pair")
          }
        }
      }
    }

    // Return the pattern
    pattern
  }

  /** Return a tuple of indices from XML
   * @param pat the XML-formatted pattern to scan
   * @param rng the range to scan for
   * @param defs a map of constant definitions in the architecture
   * @return a tuple of (start index, end index)
   */
  private def _extractIndices(pat: Node, rng: String, defs: Map[String, Int])(implicit conf: cgragen.Parameters) = {
    // Range should be specified
    if ((pat \ s"@$rng").isEmpty) {
      println(s"[ERROR] Pattern is missing its range ($rng)")
      throw new MissingDataException("missing range in pattern")
    }

    // Extract range and check that it consists of integers or defined constants
    val range = (pat \@ rng).split(' ')
    if (range.length != 2) {
      println(s"[ERROR] Pattern range ($rng) has incorrect number of arguments")
      throw new MissingDataException("invalid range in pattern")
    } else if (range.exists(arg => arg.toIntOption == None && !defs.contains(arg))) {
      println(s"[ERROR] Pattern range ($rng) has invalid argument")
      throw new MissingDataException("invalid range in pattern")
    }

    // Find the values of the range entries
    val startInd = range.head.toIntOption.getOrElse(defs(range.head))
    val endInd   = range.last.toIntOption.getOrElse(defs(range.last))

    // Check that the indices are non-decreasing
    if (startInd > endInd) {
      println(s"[ERROR] Pattern range ($rng) has decreasing indices")
      throw new MissingDataException("invalid range in pattern")
    }
    (startInd, endInd)
  }

  /** Parse and add all sub-modules from XML to the passed pattern
   * @param pat the XML-formatted pattern to scan
   * @param row the current row index
   * @param col the current column index
   * @param pattern the pattern to add sub-modules to
   */
  private def _addSubModules(
    pat    : Node, 
    row    : Int, 
    col    : Int, 
    pattern: Pattern
  )(
    implicit conf: cgragen.Parameters
  ): Unit = {
    var (currentRow, currentCol) = (row, col)
    (pat \ "block").foreach { block =>
      // Blocks must specify a module type
      if ((block \ "@module").isEmpty) {
        println(s"[ERROR] Pattern has sub-module without module type")
        throw new MissingDataException("missing module type")
      }
      val blockType = block \@ "module"

      // Generate a name for this block
      val blockName = s"block_${currentRow}_${currentCol}"

      // Add the block to the pattern
      pattern.addSubModule(blockName, blockType)
      if (conf.CGRADebug) {
        println(s"[DEBUG] Added block ($blockName) as sub-module of type ($blockType) to pattern")
      }

      // Update row and column counters
      currentCol += 1
      if (currentCol > pattern.endCol) {
        currentCol  = col
        currentRow += pattern.rowInc
      }
    }
  }

  /** Given a string with a potential mathematical statement in it, 
   * calculate the result of it and replace it in the string
   * @param idtf the connection argument
   * @return the collapsed connection argument
   */
  private def _collapseMath(idtf: String): String = {
    (prodRegex.findFirstIn(idtf), sumRegex.findFirstIn(idtf)) match {
      case (Some(prod), _) =>
        val prodRegex(op1, op2) = prod
        _collapseMath(idtf.replaceFirst(op1 + """\*""" + op2, (op1.trim.toInt * op2.trim.toInt).toString))
      case (None, Some(sum)) =>
        val sumRegex(op1, op2) = sum
        _collapseMath(idtf.replaceFirst(op1 + """\*""" + op2, (op1.trim.toInt + op2.trim.toInt).toString))
      case (None, None) =>
        idtf
    }
  }

  /** Given a relative block identifier, calculate its proper identifier 
   * and replace it in the string
   * @param idtf the relative block identifier
   * @return the proper identifier referred to relatively
   */
  private def _collapseRelative(idtf: String, row: Int, col: Int, pattern: Pattern) = {
    idtf.split('.').toList match {
      case equation :: portName :: Nil =>
        // Find and wrap row and column offsets as required
        val (rowOffset, colOffset) = {
          // Remove outer parentheses leaving only the space-separated 
          // offsets and fetch them
          val offsets = equation.drop(5).dropRight(1).split(' ').map(_.trim.toInt)
          if (offsets.length != 2) {
            print("[ERROR] Pattern has relative block identifier with incorrect ")
            println("number of offsets")
            throw new DuplicateDataException("incorrect number of offsets")
          }

          // Depending on whether wrap-around is enabled, correct the 
          // offsets accordingly
          val _colOffset = if (pattern.doWrapCol) {
            val colDiff = offsets.last
            if (colDiff + col > pattern.endCol)
              (colDiff % (pattern.endCol + 1)) - (pattern.endCol + 1)
            else if (colDiff + col < 0)
              (colDiff % (pattern.endCol + 1) + pattern.endCol + 1) % (pattern.endCol + 1)
            else
              colDiff
          } else offsets.last
          val _rowOffset = if (pattern.doWrapRow) {
            val rowDiff = offsets.head
            if (rowDiff + row > pattern.endRow) 
              (rowDiff % (pattern.endRow + 1)) - (pattern.endRow + 1) 
            else if (rowDiff + row < 0) 
              (rowDiff % (pattern.endRow + 1) + pattern.endRow + 1) % (pattern.endRow + 1) 
            else 
              rowDiff
          } else offsets.head
          (_rowOffset, _colOffset)
        }

        // Return the proper block name with the true indices
        s"block_${row+rowOffset}_${col+colOffset}.$portName"

      case _ =>
        // The argument has too many or too few hierarchical levels
        println(s"[ERROR] Pattern has connection with invalid argument ($idtf)")
        throw new MissingDataException("invalid connection argument")
    }
  }
}
