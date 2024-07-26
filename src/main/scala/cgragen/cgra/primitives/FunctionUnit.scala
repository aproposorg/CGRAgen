package cgragen.cgra.primitives

import cgragen.archparse.PortType.{PortInput, PortOutput}

import cgragen.cgra._, AbstractOperation.isApproximable
import cgragen.cgra.ModuleType.ModPrimFunc

import cgragen.dfgparse.Opcode.{Opcode, OpConst}

/** Abstract function unit module
 * @param name the name of the module
 * @param approx whether this module supports approximation
 * @param operations the operations supported by the module
 * 
 * @note Inherits from [[AbstractModule]].
 */
private[cgragen] final class AbstractFunctionUnit(name: String, dataSize: Int,
  val approx: Boolean, val operations: Seq[AbstractOperation])
  (implicit conf: cgragen.Parameters) extends AbstractModule(name, dataSize, ModPrimFunc) {
  require(operations.size >= 1,
    "function unit must support at least one operation")
}

private[cgragen] object AbstractFunctionUnit {
  /** Create a new AbstractFunctionUnit with the specified arguments
   * @param name the name of the module
   * @param ops the operations supported by the module
   * @param iis the initiation intervals of the supported operations
   * @param lats the latencies of the supported operations
   * @param approx whether this module supports approximation (defaults to false)
   * @param size the size of the module's internals (defaults to cgra.DataSize)
   * @return a new instance of [[AbstractFunctionUnit]]
   */
  def apply(name: String, ops: Seq[Opcode], iis: Seq[Int], lats: Seq[Int],
    approx: Boolean = false, size: Int = -1)
    (implicit conf: cgragen.Parameters): AbstractFunctionUnit = {
    // Get default parameters if none are passed
    val fuOps  = if (ops.isEmpty ) tokenizeOpList(conf.FUArgs("Ops")) else ops
    val fuIIs  = if (iis.isEmpty ) Seq.fill(fuOps.size)(conf.FUArgs("II").toInt) else iis
    val fuLats = if (lats.isEmpty) Seq.fill(fuOps.size)(conf.FUArgs("Latency").toInt) else lats
    val fuSize = if (size < 0) conf.DataSize else size

    // Validate that the function unit has not been assigned the constant operation
    if (fuOps.contains(OpConst)) {
      println(s"[ERROR] Function unit cannot implement constant operation")
      throw new Exception("invalid operation")
    }

    // Validate that all IIs and latencies are possible values
    if (!fuIIs.forall(_ > 0)) {
      println(s"[ERROR] Function unit operations must have positive II")
      throw new Exception("invalid II")
    }
    if (!fuLats.forall(_ >= 0)) {
      println(s"[ERROR] Function unit operations must have non-negative latency")
      throw new Exception("invalid latency")
    }

    val fu = new AbstractFunctionUnit(name, fuSize, approx, (0 until fuOps.size).map { i =>
      new AbstractOperation(fuSize, fuOps(i), fuIIs(i), fuLats(i)) })

    // Add some ports
    fu.addPort("in_a", PortInput, fuSize)
    fu.addPort("in_b", PortInput, fuSize)
    fu.addPort("out", PortOutput, fuSize)
    if (fuOps.length > 1) {
      // Add select and output ports if more than one operation is supported
      val muxName = s"${name}_mux"
      val muxSize = fuOps.size
      val mux = new AbstractMultiplexerUnit(muxName, fuSize, muxSize)
      (0 until fuOps.length).foreach(i => mux.addPort(s"in$i", PortInput, fuSize))
      mux.addPort("select", PortInput, log2Ceil(muxSize))
      mux.addPort("out", PortOutput, fuSize)

      fu.addSubModule(mux)
      fu.addPort("select", PortInput, log2Ceil(muxSize))
      fu.addConfig(new ConfigCell(s"${name}_select"), "this.select")
      fu.addConnection("this.select", s"$muxName.select")
      fu.addConnection(s"$muxName.out", "this.out")
    }
    if (approx) {
      // Add a mode select port if an approximable operation is supported
      if (fuOps.exists(isApproximable(_))) {
        fu.addPort("mode", PortInput, log2Ceil(conf.CGRAApproximationModes + 1))
        fu.addConfig(new ConfigCell(s"${name}_mode"), "this.mode")
      }
    }

    // Add some modules implementing the modes
    fuOps.zipWithIndex.foreach { case (op, i) =>
      val subModName         = s"op_${op}"
      val subModTemplateName = s"${op}_ii${fuIIs(i)}_lat${fuLats(i)}_${fuSize}b"
      val subMod = new AbstractBaseModule(subModName, fuSize, subModTemplateName)
      subMod.addPort("in_a", PortInput, fuSize)
      subMod.addPort("in_b", PortInput, fuSize)
      subMod.addPort("out", PortOutput, fuSize)
      fu.addSubModule(subMod)

      fu.addConnection("this.in_a", s"$subModName.in_a")
      fu.addConnection("this.in_b", s"$subModName.in_b")
      if (fuOps.size > 1) {
        fu.addConnection(s"$subModName.out", s"${name}_mux.in$i")
      } else {
        fu.addConnection(s"$subModName.out", "this.out")
      }
    }

    fu
  }
}
