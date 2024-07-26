package cgragen.hwgen

import cgragen.dfgparse.Opcode._

trait HWSpecUtils {

  final val MaxDataSize  = 64
  final val NoConfs      = 10
  final val SupportedOps = Seq(OpAdd, OpSub, OpMul, OpDiv, OpAnd, OpOr, OpXor, OpShl, OpShra, OpShrl)

  // We use a random number generator to generate random configurations of 
  // the test primitives
  val rng = new scala.util.Random(42)

  // Regex string to match any Verilog module
  final val anyModuleRegex = "module\\s+(\\w+)\\s*\\("

  /** Generate a regex string for a Verilog module with a given name
   * @param name name of the module
   * @return a regex string matching a Verilog module declaration
   */
  def moduleRegex(name: String): String = s"""module\\s+$name"""

  /** Generate a regex string for a Verilog port with a given width and name
   * @param dir direction of the port (should be one of "in" or "out")
   * @param name name of the port
   * @param width width of the port
   * @return a regex string matching a Verilog port declaration
   */
  def portRegex(dir: String, name: String, width: Int): String = {
    require(dir == "in" || dir == "out", "ports should be either inputs or outputs")
    require(width >= 1, "width should be positive")
    if (width == 1) {
      s"""${dir}put\\s*${name}"""
    } else {
      s"""${dir}put\\s*\\[${width-1}:0\\]\\s*${name}"""
    }
  }

  /** Generate a regex string for a Verilog wire/reg with a given width and name
   * @param name name of the wire/reg
   * @param width of the wire/reg
   * @return a regex string matching a Verilog wire/reg declaration
   */
  def signalRegex(name: String, width: Int): String = {
    require(width >= 1, "width should be positive")
    if (width == 1) {
      s"""(wire|reg)\\s*${name}"""
    } else {
      s"""(wire|reg)\\s*\\[${width-1}:0\\]\\s*${name}"""
    }
  }
}
