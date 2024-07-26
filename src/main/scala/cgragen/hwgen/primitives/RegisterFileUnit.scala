package cgragen.hwgen.primitives

import cgragen.cgra.primitives.AbstractRegisterFileUnit

import chisel3._

/** Hardware register file
 * @param mod the AbstractRegisterFileUnit to generate the hardware from
 */
private[hwgen] final class RegisterFile(mod: AbstractRegisterFileUnit)
  (implicit conf: cgragen.Parameters) extends Primitive(mod) {
  // Generate the registers needed first
  val regs = withReset(reset.asBool || clr) {
    Reg(Vec(1 << mod.numRegsLg2, UInt(mod.dataSize.W)))
  }

  // Then generate the input ports
  val dataIns = io.ins.elements.keys.toSeq.sorted
  val addrIns = ctrl.ins.elements.keys.filter(_.startsWith("addr_in")).toSeq.sorted
  val weIns   = ctrl.ins.elements.keys.filter(_.startsWith("WE")).toSeq.sorted
  dataIns.zip(addrIns.zip(weIns)).foreach { case (dataInField, (addrInField, weInField)) =>
    (dataInField, addrInField, weInField) match {
      case (s"in$i", s"addr_in$j", s"WE$k") if (i == j && j == k) =>
        when(en & ctrl.ins(weInField).orR()) {
          regs(ctrl.ins(addrInField)) := io.ins(dataInField)
        }
      case _ =>
        println("[ERROR] Register file inputs must have data, address and WE ports with the same index")
        throw new Exception(s"unmatched input ports $dataInField, $addrInField, and $weInField in register file")
    }
  }

  // And finally, generate the output ports
  val dataOuts = io.outs.elements.keys.toSeq.sorted
  val addrOuts = ctrl.ins.elements.keys.filter(_.startsWith("addr_out")).toSeq.sorted
  dataOuts.zip(addrOuts).foreach { case (dataOutField, addrOutField) =>
    (dataOutField, addrOutField) match {
      case (s"out$i", s"addr_out$j") if (i == j) =>
        io.outs(dataOutField) := regs(ctrl.ins(addrOutField))
      case _ =>
        println(s"[ERROR] Register file outputs must have data and address ports with the same index")
        throw new Exception(s"unmatched output ports $dataOutField and $addrOutField in register file")
    }
  }
}
