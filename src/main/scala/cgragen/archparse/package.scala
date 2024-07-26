package cgragen

package object archparse {
  /** Some new exception classes */
  private[archparse] class MisplacedFieldException(msg: String) extends Exception(msg)
  private[archparse] class MissingFieldException(msg: String) extends Exception(msg)
  private[archparse] class DuplicateFieldException(msg: String) extends Exception(msg)
  private[archparse] class MissingDataException(msg: String) extends Exception(msg)
  private[archparse] class NegativeDataSizeException(msg: String) extends Exception(msg)
  private[archparse] class DuplicateDataException(msg: String) extends Exception(msg)
  private[archparse] class DuplicateDefinitionException(msg: String) extends Exception(msg)
  private[archparse] class InvalidIdentifierException(msg: String) extends Exception(msg)
  private[archparse] class RecursiveDefinitionException(msg: String) extends Exception(msg)

  /** Set of Verilog HDL 1995 keywords */
  private[archparse] final val vlog1995KWs = Set("always", "ifnone", "rpmos", 
    "and", "initial", "rtran", "assign", "inout", "rtranif0", "begin", 
    "input", "rtranif1", "buf", "integer", "scalared", "bufif0", "join", 
    "small", "bufif1", "large", "specify", "case", "macromodule", "specparam", 
    "casex", "medium", "strong0", "casez", "module", "strong1", "cmos", "nand", 
    "supply0", "deassign", "negedge", "supply1", "default", "nmos", "table", 
    "defparam", "nor", "task", "disable", "not", "time", "edge", "notif0", 
    "tran", "else", "notif1", "tranif0", "end", "or", "tranif1", "endcase", 
    "output", "tri", "endmodule", "parameter", "tri0", "endfunction", "pmos", 
    "tri1", "endprimitive", "posedge", "triand", "endspecify", "primitive", 
    "trior", "endtable", "pull0", "trireg", "endtask", "pull1", "vectored", 
    "event", "pullup", "wait", "for", "pulldown", "wand", "force", "rcmos", 
    "weak0", "forever", "real", "weak1", "fork", "realtime", "while", 
    "function", "reg", "wire", "highz0", "release", "wor", "highz1", "repeat", 
    "xnor", "if", "rnmos", "xor")

  /** Set of extra Verilog 2001 keywords */
  private[archparse] final val vlog2001KWs = Set("automatic", "incdir", 
    "pulsestyle_ondetect", "cell", "include", "pulsestyle_onevent", 
    "config", "instance", "signed", "endconfig", "liblist", "showcancelled", 
    "endgenerate", "library", "unsigned", "generate", "localparam", "use", 
    "genvar", "noshowcancelled")

  /** Check that a name satisfies Verilog's naming conventions
   * @param name the name to check
   * @return true if the name satisfies the conventions; false otherwise
   * 
   * @note According to Intel's AHDL, VHDL and Verilog HDL naming conventions, 
   *       identifiers cannot contain '/' or '-', but may contain '$'. 
   *       Identifiers cannot begin with a digit or '$'. They can also not be 
   *       Verilog keywords. Identifiers are limited to 1024 characters.
   */
  private[archparse] def isValidName(name: String) = {
    val hasSlash = name.contains('/')
    val hasDash  = name.contains('-')
    val startDollar = name.startsWith("$")
    val startDigit  = (0 until 10)
      .map { i => name.startsWith(i.toString()) }
      .foldLeft(false) { case (acc, elem) => acc || elem }
    val is1995KW = vlog1995KWs.contains(name)
    val is2001KW = vlog2001KWs.contains(name)
    val isTooLong = name.length > 1024
    !hasSlash && !hasDash && !startDollar && !startDigit && !is1995KW && !is2001KW && !isTooLong
  }

  // Port type enumeration
  private[cgragen] object PortType extends Enumeration {
    type PortType       = Value
    val PortInput       = Value("input")
    val PortOutput      = Value("output")
    val PortUnspecified = Value("unspecified")
  }

  // Connection type case class
  private[cgragen] case class Connection(
    toType   : String,
    toPorts  : String,
    fromType : String,
    fromPorts: String,
    sext     : Boolean
  )
}
