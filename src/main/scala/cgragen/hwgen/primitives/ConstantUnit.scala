package cgragen.hwgen.primitives

import cgragen.cgra.primitives.AbstractConstantUnit

/** Hardware constant unit
 * @param mod the AbstractConstantUnit to generate the hardware from
 */
private[hwgen] final class ConstantUnit(mod: AbstractConstantUnit)
  (implicit conf: cgragen.Parameters) extends Primitive(mod) {
  require(ctrl.ins.elements.size == 1 && ctrl.outs.elements.size == 0,
    "constant unit may contain only one control input")
  require(io.ins.elements.size == 0 && io.outs.elements.size == 1,
    "constant unit may contain only one data output")

  val (constName, constPort) = ctrl.ins.elements.head
  val (outName, outPort)     = io.outs.elements.head
  outPort.suggestName(outName) := constPort.suggestName(constName)
}
