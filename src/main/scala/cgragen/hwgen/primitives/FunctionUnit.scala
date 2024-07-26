package cgragen.hwgen.primitives

import cgragen.cgra.isHWApproximable
import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse.Opcode._

import approx.addition.AdaptiveOFLOCA
import approx.multiplication.AdaptiveRadix2Multiplier

import chisel3._
import chisel3.util.{MuxLookup, RegEnable, log2Up}

/** Hardware function unit
 * @param mod the AbstractFunctionUnit to generate the hardware from
 */
private[hwgen] final class FunctionUnit(mod: AbstractFunctionUnit)
  (implicit conf: cgragen.Parameters) extends Primitive(mod) {
  require(ctrl.ins.elements.size <= 2 && ctrl.outs.elements.size == 0,
    "function unit may contain at most two control inputs")
  require(io.ins.elements.size == 2 && io.outs.elements.size == 1,
    "function unit may contain only two data inputs and one data output")

  // For now, we assume all operations have initiation interval equal to 1
  assume(mod.operations.forall { _.ii == 1 },
    "function unit currently only supports initiation interval 1")

  /** Parameterizable hardware operation
   * @param op the opcode of the operation
   * @param ii the initiation interval of the operation
   * @param lat the latency of the operation
   * @param approx whether this operation implements approximation (defaults to false)
   */
  private class Operation(op: Opcode, ii: Int, lat: Int, approx: Boolean = false)
    extends Module {
    override val desiredName = s"${op}_ii${ii}_lat${lat}"

    // General IO
    private val _modeW = log2Up(conf.CGRAApproximationModes+1)
    val io = IO(new Bundle {
      val in_a = Input(UInt(mod.dataSize.W))
      val in_b = Input(UInt(mod.dataSize.W))
      val mode = if (approx) Some(Input(UInt(_modeW.W))) else None
      val out  = Output(UInt(mod.dataSize.W))
    })

    // Enable and clear inputs for the data path
    val en  = IO(Input(Bool()))
    val clr = IO(Input(Bool()))

    /** Pipeline a signal to a specified depth
     * @param sig the signal to pipeline
     * @param lat the depth of the pipeline
     * @return the final output of the pipeline
     */
    private def _pipe(sig: UInt, lat: Int): UInt = withReset(reset.asBool || clr) {
      (0 until lat).foldLeft(sig) { case (acc, _) => RegEnable(acc, 0.U, en) }
    }

    // Generate the hardware for the specified operation
    private val _sftW = log2Up(mod.dataSize)
    private val _approxW  = (conf.CGRAApproximationWidthFraction * mod.dataSize).toInt
    private val _numModes = conf.CGRAApproximationModes
    private val _opOut = op match {
      case OpAdd =>
        if (approx) {
          val aofloca = Module(new AdaptiveOFLOCA(mod.dataSize, _approxW, _numModes))
          aofloca.io.a    := io.in_a
          aofloca.io.b    := io.in_b
          aofloca.io.cin  := false.B
          aofloca.io.ctrl := io.mode.get
          aofloca.io.s
        } else io.in_a + io.in_b
      case OpSub =>
        if (approx) {
          val aofloca = Module(new AdaptiveOFLOCA(mod.dataSize, _approxW, _numModes))
          aofloca.io.a    := io.in_a
          aofloca.io.b    := ~io.in_b
          aofloca.io.cin  := true.B
          aofloca.io.ctrl := io.mode.get
          aofloca.io.s
        } else io.in_a - io.in_b
      case OpMul =>
        if (approx) {
          val ar2m = Module(
            new AdaptiveRadix2Multiplier(mod.dataSize, mod.dataSize,
              _approxW, true, true, true, numModes=_numModes))
          ar2m.io.a    := io.in_a
          ar2m.io.b    := io.in_b
          ar2m.io.ctrl := io.mode.get
          ar2m.io.p
        } else io.in_a * io.in_b
      case OpDiv  => io.in_a / io.in_b
      case OpAnd  => io.in_a & io.in_b
      case OpOr   => io.in_a | io.in_b
      case OpXor  => io.in_a ^ io.in_b
      case OpShl  => io.in_a << io.in_b(_sftW-1, 0)
      case OpShra => io.in_a.asSInt >> io.in_b(_sftW-1, 0)
      case OpShrl => io.in_a        >> io.in_b(_sftW-1, 0)
      case _ =>
        throw new Exception(s"cannot generate hardware for operation $op")
    }

    // Delay the output of the operation by appending registers to it
    io.out := _pipe(_opOut.asUInt, lat)
  }

  // Generate all the operation hardware and connect their inputs
  val (inAName, inAPort) = io.ins.elements.head
  val (inBName, inBPort) = io.ins.elements.last
  private val opOuts = mod.operations.map { oprtn =>
    // Make sure to use the pre-instantiated modules when possible
    val approx = mod.approx && isHWApproximable(oprtn.op)
    val opMod  = Module(new Operation(oprtn.op, oprtn.ii, oprtn.lat, approx))

    // Connect the inputs of the operation module
    opMod.io.in_a := inAPort.suggestName(inAName)
    opMod.io.in_b := inBPort.suggestName(inBName)
    opMod.en  := en
    opMod.clr := clr
    opMod.io.mode match {
      case Some(port) => port := ctrl.ins("mode").suggestName("mode")
      case _ => // no action needed
    }

    // Return the output of the operation module
    opMod.io.out
  }

  // If the function unit contains more than one operation, implement a 
  // multiplexer to select between them. Otherwise, simply connect the 
  // operation's output
  val (outName, outPort) = io.outs.elements.head
  if (opOuts.size == 1) {
    outPort.suggestName(outName) := opOuts.head
  } else {
    io.outs.elements.head._2 := MuxLookup(ctrl.ins("select").suggestName("select"),
      DontCare, opOuts.zipWithIndex.map { case (out, i) => i.U -> out }.toSeq
    )
  }
}
