package cgragen

import scala.collection.mutable

package object dfgparse {  
  /** Some new exception classes */
  private[dfgparse] class DFGParseException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGMissingNodeException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGMissingOperandException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGDuplicateDefinitionException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGMissingOpcodeException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGWrongNumberOfOperandsException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGInvalidConstantException(msg: String) extends Exception(msg)
  private[dfgparse] class DFGInvalidEdgeException(msg: String) extends Exception(msg)

  // DFG opcode enumeration
  private[cgragen] object Opcode extends Enumeration {
    type Opcode = Value
    //val OpSext        = Value("sext")
    //val OpZext        = Value("zext")
    //val OpTrunc       = Value("trunc")
    val OpInput       = Value("input")
    val OpOutput      = Value("output")
    val OpConst       = Value("const")
    val OpAdd         = Value("add")
    val OpSub         = Value("sub")
    val OpMul         = Value("mul")
    val OpDiv         = Value("div")
    val OpAnd         = Value("and")
    val OpOr          = Value("or")
    val OpXor         = Value("xor")
    val OpShl         = Value("shl")
    val OpShra        = Value("shra")
    val OpShrl        = Value("shrl")
    val OpLoad        = Value("load")
    val OpStore       = Value("store")
    val OpUnspecified = Value("unspecified")
  }
  import Opcode._

  // Sets of different types of operations
  private[dfgparse] final val arithOps = Set(OpAdd, OpSub, OpMul, OpDiv)
  private[dfgparse] final val logicOps = Set(OpAnd, OpOr, OpXor, OpShl, OpShra, OpShrl)

  /** Check whether an operation is arithmetic
   * @param op the operation to check
   * @return true iff `op` is an arithmetic operation
   */
  private[cgragen] def isArith(op: Opcode): Boolean = arithOps(op)

  /** Check whether an operation is logical
   * @param op the operation to check
   * @return true iff `op` is a logical operation
   */
  private[cgragen] def isLogic(op: Opcode): Boolean = logicOps(op)

  // Encoding of opcodes to their required number of operands
  private[dfgparse] final val reqOperands = {
    val nonary = Seq(OpInput, OpConst, OpUnspecified)
    val unary  = Seq(OpOutput, OpLoad, OpStore)
    val binary = (Opcode.values -- nonary -- unary).toSeq
    (nonary.map(_ -> 0) ++ unary.map(_ -> 1) ++ binary.map(_ -> 2)).toMap
  }
}
