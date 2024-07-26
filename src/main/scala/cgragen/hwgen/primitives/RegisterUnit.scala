package cgragen.hwgen.primitives

import cgragen.cgra.primitives.AbstractRegisterUnit

import chisel3._
import chisel3.util.RegEnable

/** Hardware register
 * @param mod the AbstractRegisterUnit to generate the hardware from
 */
private[hwgen] final class Register(mod: AbstractRegisterUnit)
  (implicit conf: cgragen.Parameters) extends Primitive(mod) {
  require(ctrl.ins.elements.size == 0 && ctrl.outs.elements.size == 0,
    "register may contain no control inputs or outputs")
  require(io.ins.elements.size == 1 && io.outs.elements.size == 1,
    "register may contain only one data input and one data output")

  withReset(reset.asBool || clr) {
    io.outs.elements.head._2 := RegEnable(io.ins.elements.head._2, 0.U, en)
  }
}
