package cgragen.hwgen.primitives

import cgragen.cgra.primitives.AbstractMultiplexerUnit

import chisel3._
import chisel3.util.MuxLookup

/** Hardware multiplexer
 * @param mod the AbstractMultiplexerUnit to generate the hardware from
 */
private[hwgen] final class Multiplexer(mod: AbstractMultiplexerUnit)
  (implicit conf: cgragen.Parameters) extends Primitive(mod) {
  require(ctrl.ins.elements.size == 1 && ctrl.outs.elements.size == 0,
    "multiplexer may contain only one control input")
  require(io.outs.elements.size == 1,
    "multiplexer may contain only one data output")

  val (selName, selPort) = ctrl.ins.elements.head
  io.outs.elements.head._2 := MuxLookup(selPort.suggestName(selName), DontCare,
    io.ins.elements.keys.toSeq.sorted.zipWithIndex.map { case (field, i) =>
      i.U -> io.ins(field) })
}
