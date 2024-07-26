package cgragen.hwgen

import cgragen.chiselTestAnnos

import cgragen.cgra.primitives.AbstractFunctionUnit

import cgragen.dfgparse.Opcode._

import cgragen.hwgen.primitives.FunctionUnit

import scala.sys.process._

import chisel3._

import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec

class ApproximateFunctionUnitSpec extends PrimitiveSpec {
  behavior of "Generated approximate function unit"

  import models.FunctionUnitModel

  final val ApproxmblOps  = Seq(OpAdd, OpSub, OpMul)
  final val AdditionalOps = Seq(OpDiv, OpAnd, OpOr, OpXor, OpShl, OpShra, OpShrl)
  final val MaxLatency    = 5

  rng.shuffle(8 to (MaxDataSize / 4)).take(NoConfs / 2).foreach { dataSize =>
    val (numApprxmbl, numAdd) = (rng.nextInt(ApproxmblOps.size) + 1,
                                 rng.nextInt(AdditionalOps.size) + 1)
    val numOps = numApprxmbl + numAdd
    val (apprxmblOps, addOps) = (rng.shuffle(ApproxmblOps).take(numApprxmbl),
                                 rng.shuffle(AdditionalOps).take(numAdd))
    val ops = apprxmblOps ++ addOps
    val iis    = Seq.fill(numOps)(1) // @todo randomize these when supported!
    val lats   = Seq.fill(numOps) { rng.nextInt(MaxLatency) }

    it should s"work at width $dataSize" in {
      val abstractFU = AbstractFunctionUnit(s"func_${dataSize}b", ops, iis, lats, true, dataSize)

      // Create a software model of the function unit
      val modelFU = new FunctionUnitModel(abstractFU)

      test(new FunctionUnit(abstractFU))
        .withAnnotations(chiselTestAnnos) { dut =>
        // Zero-out all inputs to the function unit
        dut.en .poke(true.B)
        dut.clr.poke(false.B)
        dut.io  .ins.elements.keys.foreach { name => dut.io  .ins(name).poke(0.U) }
        dut.ctrl.ins.elements.keys.foreach { name => dut.ctrl.ins(name).poke(0.U) }

        modelFU.en .poke(true)
        modelFU.clr.poke(false)

        // Execute a number of randomly selected operations
        (0 until NoOps).foreach { _ =>
          // Generate the random inputs and pass them to the DUT
          val dinA = BigInt(dataSize, rng)
          val dinB = BigInt(dataSize, rng)
          val sel = if (ops.size > 1) Some(rng.nextInt(ops.size)) else None
          
          val inAPort = "in_a"
          dut.io.ins(inAPort).poke(dinA.U)
          modelFU.io.ins.poke(inAPort, dinA)

          val inBPort = "in_b"
          dut.io.ins(inBPort).poke(dinB.U)
          modelFU.io.ins.poke(inBPort, dinB)

          val selPort = "select"
          sel match {
            case Some(selIn) =>
              dut.ctrl.ins(selPort).poke(selIn.U)
              modelFU.ctrl.ins.poke(selPort, selIn)
            case _ =>
          }

          // Advance the simulation time
          dut.clock.step()
          modelFU.step()

          // Check that the output matches the model
          if (sel != None && ops(sel.get) != OpDiv)
            dut.io.outs("out").expect(modelFU.io.outs.peek("out").U)
        }
      }
    }
  }
}
