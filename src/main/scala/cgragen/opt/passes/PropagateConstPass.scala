package cgragen.opt.passes

import cgragen.genUniqueName

import cgragen.cgra.{AbstractModule, CGRA}
import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse.{DFG, DFGEdge, DFGNode, isArith, isLogic}
import cgragen.dfgparse.Opcode._

import cgragen.opt.Pass

import chisel3.util.{isPow2, log2Up}

import scala.collection.mutable

/** Propagate constant zeros and powers-of-two
 * 
 * @note Powers-of-two are only propagated if they feed multiplications and 
 *       the target architecture supports shifting.
 */
private[cgragen] case object PropagateConstPass
  extends Pass(Seq.empty[Pass], Seq(RemoveDeadPass)) {
  private[cgragen] class PropagateConstException(msg: String) extends Exception(msg)

  /** Check whether a CGRA supports a particular operation
   * @param cgra the CGRA to search through
   * @param op the operation to search for
   * @return true iff `cgra` contains a function unit that supports `op`
   */
  private def _supportsOp(cgra: CGRA, op: Opcode): Boolean = {
    // Recursively search through the CGRA's sub-modules to find any function 
    // units it may contain
    def _helper(mod: AbstractModule): Boolean = mod match {
      case fu: AbstractFunctionUnit => fu.operations.exists(_.op == op)
      case _ => mod.subModules.foldLeft(false) { case (acc, (_, subMod)) =>
        acc || _helper(subMod) }
    }
    cgra.subModules.foldLeft(false) { case (acc, (_, block)) =>
      acc || _helper(block) }
  }

  /** Find optimizable constant nodes in a DFG
   * @param dfg the DFG to search through
   * @param cgra the targeted CGRA
   * @return all optimizable constant nodes in `dfg`
   * 
   * @note For now, this method only checks for the following conditions:
   *       - zeros driving arithmetic and logic operations
   *       - powers-of-two driving multiplications (if the CGRA supports shift)
   */
  private def _optmzblNodes(dfg: DFG, cgra: CGRA): Seq[DFGNode] = {
    val ret = mutable.HashSet.empty[DFGNode]

    // Find constant zeros driving arithmetic and logic operations
    ret ++= dfg.nodes
      .filter(node => node.opcode == OpConst && node.attrs("value").toInt == 0)
      .filter { zero =>
        dfg.edges
          .exists { edge =>
            edge.src == zero &&
            (isArith(edge.snk.opcode) ||
             isLogic(edge.snk.opcode))
          }}

    // Find powers-of-two driving multiplications
    if (_supportsOp(cgra, OpShl)) {
      ret ++= dfg.nodes
        .filter(node => node.opcode == OpConst && isPow2(node.attrs("value").toInt))
        .filter { pow2 =>
          dfg.edges
            .exists { edge =>
              edge.src == pow2 &&
              edge.snk.opcode == OpMul &&
              edge.operand == 1
            }}
    }

    ret.toSeq
  }

  def apply(dfg: DFG, cgra: CGRA)(implicit conf: cgragen.Parameters): Unit = {
    if (conf.DFGDebug) println(s"[DEBUG] Propagating constants in DFG")

    // Find all constant nodes in the DFG and iterate on the DFG till 
    // no more constants are easily propagated
    val remNodes  = mutable.ArrayBuffer.empty[DFGNode]
    val unOptmzbl = mutable.ArrayBuffer.empty[(DFGNode, DFGNode)]

    // Propagate at most one constant node per recursive call
    def _helper(dfg: DFG): Unit = {
      // Keep track of changes local to this iteration
      val changed = mutable.ArrayBuffer.empty[DFGNode]

      // Pass an operand through over a constant zero
      def _passOperand(zero: DFGNode, op: DFGNode): Unit = {
        // Remove the target node from the DFG
        dfg.removeNode(op)
        remNodes += op

        // Re-route edges into the target node to its outputs
        val ins = dfg.edges
          .filter(edge => edge.snk == op && edge.src != zero)
        val outs = dfg.edges.filter(_.src == op)
        (ins ++ outs).foreach(dfg.removeEdge(_))
        ins.map(_.src)
          .foreach { src =>
            outs.foreach { edge =>
              dfg.addEdge(new DFGEdge(src, edge.snk, edge.operand)) }
          }
        changed ++= ins.map(_.src) ++ outs.map(_.snk)

        // Remove the edge from the constant node to the target node
        val cop = dfg.edges
          .collectFirst { case edge if edge.src == zero && edge.snk == op => edge }
          .get
        dfg.removeEdge(cop)
      }

      // Pass a constant zero over an operation
      def _passZero(zero: DFGNode, op: DFGNode): Unit = {
        // Remove the target node from the DFG
        dfg.removeNode(op)
        remNodes += op

        // Remove edges into and out of the target node
        val ins  = dfg.edges.filter(_.snk == op)
        val outs = dfg.edges.filter(_.src == op)
        (ins ++ outs).foreach(dfg.removeEdge(_))
        changed ++= ins.map(_.src) ++ outs.map(_.snk)

        // Insert a constant zero node to replace the target node
        val constName = genUniqueName(s"${op.name}_const0", dfg.nodes.map(_.name))
        val const = new DFGNode(constName, OpConst)
        dfg.addNode(const)
        const.addAttr("value", "0")
        outs.foreach { edge =>
          dfg.addEdge(new DFGEdge(const, edge.snk, edge.operand)) }
      }

      // Find the first pair of optimizable nodes, if any
      // (a constant node and an arith/logic node)
      val victimOpt = _optmzblNodes(dfg, cgra)
        .collectFirst { case node if dfg.edges
          .exists(edge => edge.src == node && !unOptmzbl.contains((node, edge.snk))) => node }

      victimOpt match {
        case Some(zero) if zero.attrs("value").toInt == 0 => // constant zero
          // Find the target node
          val (op, ind) = dfg.edges
            .collectFirst { case edge if edge.src == zero &&
              !unOptmzbl.contains((zero, edge.snk)) => (edge.snk, edge.operand) }
            .get

          // Propagate the constant differently depending on the operation in 
          // the target node
          op.opcode match {
            /** Pass the other operand through */
            case OpAdd | OpOr | OpXor =>
              _passOperand(zero, op)

            /** Nullify the unique fan-in cone (relies on dead node removal) */
            case OpMul | OpAnd =>
              _passZero(zero, op)

            /** Possibly pass the other operand through */
            case OpShl | OpShra | OpShrl | OpDiv =>
              ind match {
                case 0 => // propagate the constant (as mul | and)
                  _passZero(zero, op)

                case 1 => // pass the other operand through (as add | or | xor)
                          // or throw an error
                  if (op.opcode == OpDiv) {
                    print("[ERROR] Cannot propagate constant zero across ")
                    println(s"invalid division (${op.name})")
                    throw new PropagateConstException("division by zero")
                  }
                  _passOperand(zero, op)

                case _ =>
                  print("[ERROR] Cannot propagate constant zero across node ")
                  println(s"(${op.name}) with operands beyond index 0 or 1")
                  throw new PropagateConstException("too many operands")
              }

            /** Possibly leave depending on the operand position */
            case OpSub =>
              ind match {
                case 0 => // cannot remove node as it affects signedness
                          // (unless the other node is a constant)
                  // Find the other operand to the target node
                  val secEdge = dfg.edges
                    .filter(edge => edge.snk == op && edge.src != zero)
                    .head // only one!
                  val secOp = secEdge.src

                  // If the second operand is a constant that feeds only the 
                  // target node, replace it with another constant with 
                  // opposite sign. Otherwise mark it as unoptimizable
                  if (secOp.opcode == OpConst && dfg.edges.filter(_.src == secOp).size == 1) {
                    // Remove the second operand and its edge from the DFG
                    dfg.removeNode(secOp)
                    dfg.removeEdge(secEdge)

                    // Create a replacement node and edge pair
                    val const = new DFGNode(secOp.name, OpConst)
                    secOp.attrs.foreach {
                      case (key, value) if key == "value" =>
                        const.addAttr(key, s"${-secOp.attrs("value").toInt}")
                      case (key, value) =>
                        const.addAttr(key, value)
                    }
                    dfg.addNode(const)
                    dfg.addEdge(new DFGEdge(const, op, secEdge.operand))

                    _passOperand(zero, op)
                  } else {
                    unOptmzbl += ((zero, op))
                  }

                case 1 => // pass the other operand through (as add | or | xor)
                  _passOperand(zero, op)

                case _ =>
                  print("[ERROR] Cannot propagate constant zero across node ")
                  println(s"(${op.name}) with operands beyond index 0 or 1")
                  throw new PropagateConstException("too many operands")
              }

            /** Catch remaining unoptimizable cases */
            case code =>
              print("[ERROR] Cannot propagate constant zero across node ")
              println(s"(${op.name}) with operation ($code)")
              throw new PropagateConstException("invalid operation")
          }

          // Remove the constant node if it is disconnected
          if (dfg.edges.filter(_.src == zero).isEmpty) {
            dfg.removeNode(zero)
            remNodes += zero
          }

        case Some(pow2) if isPow2(pow2.attrs("value").toInt) => // constant power-of-two
          // Find the target node
          val op = dfg.edges
            .collectFirst { case edge if edge.src == pow2 &&
              edge.snk.opcode == OpMul && edge.operand == 1 => edge.snk }
            .get

          // Remove the target node from the DFG
          dfg.removeNode(op)
          remNodes += op

          // Create a new shift-constant node pair and re-route edges into it
          val shlName = genUniqueName(s"${op.name}_shl", dfg.nodes.map(_.name))
          val shl     = new DFGNode(shlName,  OpShl)
          val constName = genUniqueName(s"${pow2.name}_lg", dfg.nodes.map(_.name))
          val const     = new DFGNode(constName, OpConst)
          const.addAttr("value", s"${log2Up(pow2.attrs("value").toInt)}")
          dfg.addNode(shl)
          dfg.addNode(const)

          dfg.addEdge(new DFGEdge(const, shl, 1))
          dfg.edges
            .filter(edge => edge.snk == op && edge.src != pow2)
            .foreach { edge =>
              dfg.removeEdge(edge)
              dfg.addEdge(new DFGEdge(edge.src, shl, edge.operand))
              changed += edge.src
            }
          dfg.removeEdge(dfg.edges.filter(edge => edge.src == pow2 && edge.snk == op).head)

          dfg.edges
            .filter(edge => edge.src == op)
            .foreach { edge =>
              dfg.removeEdge(edge)
              dfg.addEdge(new DFGEdge(shl, edge.snk, edge.operand))
              changed += edge.snk
            }

        case _ =>
          // no action needed
      }

      // Check if the DFG has changed before recursively calling the function
      if (changed.nonEmpty) {
        unOptmzbl --= unOptmzbl.filter { case (node, _) => changed.contains(node) }
        _helper(dfg)
      }
    }
    _helper(dfg)

    if (conf.DFGDebug) {
      print(s"[DEBUG] Removed a total of (${remNodes.size}) nodes by ")
      println("propagating constants in DFG")
    }
  }
}
