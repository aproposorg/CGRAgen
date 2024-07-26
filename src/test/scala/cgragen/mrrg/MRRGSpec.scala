package cgragen.mrrg

import scala.collection.mutable

import cgragen.archparse.Architecture

import cgragen.cgra.CGRA
import cgragen.cgra.primitives.{AbstractFunctionUnit, AbstractRegisterFileUnit}

import cgragen.mrrg.MRRG

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MRRGSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "CGRA-to-MRRG converter"

  it should "reflect inferred top-level IO when requested" in {
    val locParams = params.copy(cgraInferTopLevelIO = true)
    val cgra = CGRA(Architecture(ioLessArch))(locParams)

    // Test for II = 1 .. 2
    (1 to 2).foreach { II =>
      val mrrg = MRRG(cgra, II)
      mrrg.nodes should have length II

      // Look for the top-level IO
      (0 until II).foreach { cycle =>
        val nodes = mrrg.nodes(cycle).map(_._1).filter(_.contains("_inf"))
        val topLevelIO = (0 until 1).flatMap { c =>
          Seq(s"${locParams.CGRATopLevelIOName}.block_0_${c}_in1_inf",
              s"${locParams.CGRATopLevelIOName}.block_0_${c}_in1_inf.input",
              s"${locParams.CGRATopLevelIOName}.block_0_${c}_in1_inf.out",
              s"${locParams.CGRATopLevelIOName}.block_0_${c}_out_rt_inf",
              s"${locParams.CGRATopLevelIOName}.block_0_${c}_out_rt_inf.output",
              s"${locParams.CGRATopLevelIOName}.block_0_${c}_out_rt_inf.in",
              s"${locParams.CGRATopLevelIOName}.block_1_${c}_in2_inf",
              s"${locParams.CGRATopLevelIOName}.block_1_${c}_in2_inf.input",
              s"${locParams.CGRATopLevelIOName}.block_1_${c}_in2_inf.out",
              s"${locParams.CGRATopLevelIOName}.block_1_${c}_out_fu_inf",
              s"${locParams.CGRATopLevelIOName}.block_1_${c}_out_fu_inf.output",
              s"${locParams.CGRATopLevelIOName}.block_1_${c}_out_fu_inf.in")
        }
        nodes should (have size 24 and contain allElementsOf (topLevelIO))
      }
    }
  }

  /** To be run after the conversion tests. Validates that an abstract 
   * architecture can be correctly converted to its MRRG representation. 
   * The same two architectures as used in the other tests are used here 
   * as well, and the structure of the tests is the same.
   * 
   * For simplicity, we check only the first PEs (i.e., block_0_0) running 
   * through both their internals and external connections. Doing so 
   * should be sufficient to validate the architectures, as the CGRAs are 
   * homogeneous.
   */

  it should "convert a simple architecture" in {
    // First, grab the example architecture, parse it, and convert it to a CGRA
    val cgra = CGRA(Architecture("src/test/resources/archparse/simple_arch.xml"))

    // Test for failure for II = 0
    intercept[IllegalArgumentException] {
      MRRG(cgra, 0)
    }

    // Test for II = 1 .. 2
    (1 to 2).foreach { II =>
      val mrrg = MRRG(cgra, II)
      mrrg.nodes should have length II

      // Check the first PE
      val pfx = "block_0_0"
      val modes = cgra.subModules(pfx)
        .subModules("func").asInstanceOf[AbstractFunctionUnit]
        .operations
      (0 until II).foreach { cycle =>
        val prevCycle = ((cycle - 1) % II) * (cycle - 1).sign.toInt
        val nextCycle = (cycle + 1) % II
        val nodes = mrrg.nodes(cycle).filter(_._1.startsWith(pfx))
        // Simple architecture has no IIs or latencies
        modes.forall(_.ii  == params.FUArgs("II").toInt) should be (true)
        modes.forall(_.lat == params.FUArgs("Latency").toInt) should be (true)
        // 4 ins and 1 out, 1 reg, 1 FU with 3 ops, and 2 4-input muxes
        val nodeNames = {
          val ins = (0 until 4).map(in => s"$pfx.in$in")
          val inReg = Seq(s"$pfx.rgstr.in", s"$pfx.rgstr.reg", s"$pfx.rgstr.out")
          val inMuxes = Seq("mux_func_in_a", "mux_func_in_b").flatMap { mux =>
            val muxPfx = s"$pfx.$mux"
            (0 until 4).map(in => s"$muxPfx.in$in") ++ Seq(s"$muxPfx.mux", s"$muxPfx.out")
          }
          val const = Seq(s"$pfx.const.const", s"$pfx.const.out")
          val alu = Seq(s"$pfx.func.in_a", s"$pfx.func.in_b",
            s"$pfx.func.sel", s"$pfx.func.out") ++ modes.map(_.op).zipWithIndex
            .map { case (op, ind) => s"$pfx.func.fu_${op}" }
          val outMux = (0 until 2).map(in => s"$pfx.mux_rgstr_in.in$in") ++ 
            Seq(s"$pfx.mux_rgstr_in.mux", s"$pfx.mux_rgstr_in.out")
          val out = s"$pfx.out"
          ins ++ inReg ++ inMuxes ++ const ++ alu ++ outMux :+ out
        }
        nodes.keys should (have size (nodeNames.length) and contain allElementsOf (nodeNames))

        /** Input nodes */
        (0 until 4).foreach { input =>
          val inNode = nodes(s"$pfx.in$input")
          if (input == 1 || input == 2) {
            inNode.fanin should have length 1 // East and South connections
          } else {
            inNode.fanin should be (empty) // North and West connections
          }
          inNode.fanout should have length 2
          inNode.fanout should (contain (nodes(s"$pfx.mux_func_in_a.in$input")) 
                            and contain (nodes(s"$pfx.mux_func_in_b.in$input")))
        }
        
        /** Middle nodes */
        Seq("mux_func_in_a", "mux_func_in_b").foreach { mux =>
          val muxNode = nodes(s"$pfx.$mux.mux")
          muxNode.fanin should have length 4
          (0 until 4).foreach { in => 
            muxNode.fanin should contain (nodes(s"$pfx.$mux.in$in"))
          }
          val outNode = nodes(s"$pfx.$mux.out")
          muxNode.fanout should (have length 1 and contain (outNode))
          outNode.fanin  should (have length 1 and contain (muxNode))
        }
        val (inANode, inBNode) = (nodes(s"$pfx.func.in_a"), nodes(s"$pfx.func.in_b"))
        nodes(s"$pfx.mux_func_in_a.out").fanout should (have length 1 and contain (inANode))
        nodes(s"$pfx.mux_func_in_b.out").fanout should (have length 1 and contain (inBNode))
        inANode.fanin  should (have length 1 and contain (nodes(s"$pfx.mux_func_in_a.out")))
        inBNode.fanin  should (have length 1 and contain (nodes(s"$pfx.mux_func_in_b.out")))
        inANode.fanout should  have length (modes.length)
        inBNode.fanout should  have length (modes.length)
        val selNode = nodes(s"$pfx.func.sel")
        selNode.fanin should  have length (modes.length)
        modes.map(_.op).zipWithIndex.foreach { case (op, ind) =>
          val fuNode = nodes(s"$pfx.func.fu_${op}")
          inANode.fanout should contain (fuNode)
          inBNode.fanout should contain (fuNode)
          fuNode.canMapOp(op) should be (true)
          fuNode.fanin  should (have length 2 and contain (inANode) and contain (inBNode))
          fuNode.fanout should (have length 1 and contain (selNode))
        }
        val (constNode, constOutNode) = (nodes(s"$pfx.const.const"), nodes(s"$pfx.const.out"))
        val (outMuxInNode0, outMuxInNode1, outMuxNode, outMuxOutNode) = (
          nodes(s"$pfx.mux_rgstr_in.in0"), nodes(s"$pfx.mux_rgstr_in.in1"), 
          nodes(s"$pfx.mux_rgstr_in.mux"), nodes(s"$pfx.mux_rgstr_in.out")
        )
        constNode.fanin     should  be (empty)
        constNode.fanout    should (have length 1 and contain (constOutNode))
        constOutNode.fanin  should (have length 1 and contain (constNode))
        constOutNode.fanout should (have length 1 and contain (outMuxInNode0))
        val funcOutNode = nodes(s"$pfx.func.out")
        selNode.fanout      should (have length 1 and contain (funcOutNode))
        funcOutNode.fanin   should (have length 1 and contain (selNode))
        funcOutNode.fanout  should (have length 1 and contain (outMuxInNode1))
        val (regInNode, regRegNode, regOutNode) = (
          nodes(s"$pfx.rgstr.in"), nodes(s"$pfx.rgstr.reg"), nodes(s"$pfx.rgstr.out")
        )
        outMuxInNode0.fanin   should (have length 1 and contain (constOutNode))
        outMuxInNode1.fanin   should (have length 1 and contain (funcOutNode))
        outMuxInNode0.fanout  should (have length 1 and contain (outMuxNode))
        outMuxInNode1.fanout  should (have length 1 and contain (outMuxNode))
        outMuxNode.fanin      should (have length 2 and contain (outMuxInNode0) and contain (outMuxInNode1))
        outMuxNode.fanout     should (have length 1 and contain (outMuxOutNode))
        outMuxOutNode.fanin   should (have length 1 and contain (outMuxNode))
        outMuxOutNode.fanout  should (have length 1 and contain (regInNode))

        /** Output and register nodes */
        regInNode.fanin   should (have length 1 and contain (outMuxOutNode))
        regInNode.fanout  should (have length 1 and contain (regRegNode))
        regRegNode.fanin  should (have length 1 and contain (regInNode))
        regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$pfx.rgstr.out")))
        regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$pfx.rgstr.reg")))
        regOutNode.fanout should (have length 1 and contain (nodes(s"$pfx.out")))
        nodes(s"$pfx.out").fanin  should (have length 1 and contain (regOutNode))
        nodes(s"$pfx.out").fanout should  have length 2 // East and South connections
      }
    }
  }

  it should "convert a complex architecture" in {
    // First, grab the example architecture, parse it, and convert it to a CGRA
    val cgra = CGRA(Architecture("src/test/resources/archparse/complex_arch.xml"))

    // Test for failure for II = 0
    intercept[IllegalArgumentException] {
      MRRG(cgra, 0)
    }

    // Test for II = 1 .. 2
    (1 to 2).foreach { II =>
      val mrrg = MRRG(cgra, II)
      mrrg.nodes should have length II

      // Check the RF
      {
        val pfx = "block_0_0"
        (0 until II).foreach { cycle =>
          val prevCycle  = ((cycle - 1) % II) * (cycle - 1).sign.toInt
          val nextCycle  =  (cycle + 1) % II
          val (numInputs, numOutputs, numRegs) = {
            val module = cgra.subModules(pfx).subModules("rf").asInstanceOf[AbstractRegisterFileUnit]
            (module.numInputs, module.numOutputs, 1 << module.numRegsLg2)
          }
          val nodes = mrrg.nodes(cycle).filter(_._1.startsWith(pfx))
          // 4 ins and outs (for both top-level and RF submodule), 8 regs, 
          // and 32 output selects and register selects
          val nodeNames = {
            val ins       = (0 until numInputs).map(in => s"$pfx.in$in")
            val rfIns     = (0 until numInputs).map(in => s"$pfx.rf.in$in")
            val rfRegSels = (0 until numRegs).flatMap { reg =>
              (0 until numInputs).map(in => s"$pfx.rf.reg${reg}_sel$in")
            }
            val rfRegs    = (0 until numRegs).map(reg => s"$pfx.rf.reg$reg")
            val rfOutSels = (0 until numOutputs).flatMap { out =>
              (0 until numRegs).map(reg => s"$pfx.rf.out${out}_sel$reg")
            }
            val rfOuts = (0 until numOutputs).map(out => s"$pfx.rf.out$out")
            val outs   = (0 until numOutputs).map(out => s"$pfx.out$out")
            ins ++ rfIns ++ rfRegSels ++ rfRegs ++ rfOutSels ++ rfOuts ++ outs
          }
          nodes.keys should (have size (nodeNames.length) and contain allElementsOf (nodeNames))

          /** Input nodes */
          (0 until numInputs).foreach { input =>
            val (inNode, rfInNode) = (nodes(s"$pfx.in$input"), nodes(s"$pfx.rf.in$input"))
            inNode.fanin    should  have length 1
            inNode.fanout   should (have length 1 and contain (rfInNode))
            rfInNode.fanin  should (have length 1 and contain (inNode))
            rfInNode.fanout should  have length (numRegs)
            rfInNode.fanout should  contain allElementsOf ((0 until numRegs).map { reg =>
              nodes(s"$pfx.rf.reg${reg}_sel$input")
            })
          }

          /** Middle and register select nodes */
          (0 until numRegs).foreach { register =>
            val regNode = nodes(s"$pfx.rf.reg$register")
            if (II != 1) regNode.fanin should have length (numInputs + 1)
            else         regNode.fanin should have length (numInputs)
            (0 until numInputs).foreach { input =>
              val regSelNode = nodes(s"$pfx.rf.reg${register}_sel$input")
              regSelNode.fanin  should (have length 1 and contain (nodes(s"$pfx.rf.in$input")))
              regSelNode.fanout should (have length 1 and contain (regNode))
              regNode.fanin     should  contain (regSelNode)
            }
            if (II != 1) {
              regNode.fanin  should contain (mrrg.nodes(prevCycle)(s"$pfx.rf.reg$register"))
              regNode.fanout should contain (mrrg.nodes(nextCycle)(s"$pfx.rf.reg$register"))
            }
            regNode.fanout should contain allElementsOf ((0 until numOutputs).map { out =>
              mrrg.nodes(nextCycle)(s"$pfx.rf.out${out}_sel$register")
            })
          }

          /** Output and output select nodes */
          (0 until numOutputs).foreach { output =>
            val (rfOutNode, outNode) = (nodes(s"$pfx.rf.out$output"), nodes(s"$pfx.out$output"))
            rfOutNode.fanin should have length (numRegs)
            (0 until numRegs).foreach { register =>
              val outSelNode = nodes(s"$pfx.rf.out${output}_sel$register")
              outSelNode.fanin  should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$pfx.rf.reg$register")))
              outSelNode.fanout should (have length 1 and contain (rfOutNode))
            }
            rfOutNode.fanout should (have length 1 and contain (outNode))
            outNode.fanin    should (have length 1 and contain (rfOutNode))
            outNode.fanout   should have length 1
          }
        }
      }

      // Check the first PE
      {
        val pfx = "block_0_1"
        val modes = cgra.subModules(pfx)
          .subModules("alu").asInstanceOf[AbstractFunctionUnit]
          .operations
        (0 until II).foreach { cycle =>
          val prevCycle = ((cycle - 1) % II) * (cycle - 1).sign.toInt
          val nextCycle = (cycle + 1) % II
          val nodes = mrrg.nodes(cycle).filter(_._1.startsWith(pfx))
          // Complex architecture has no IIs or latencies either
          modes.forall(_.ii  == params.FUArgs("II").toInt) should be (true)
          modes.forall(_.lat == params.FUArgs("Latency").toInt) should be (true)
          // 4 ins and outs, 7 regs, 1 FU with 10 ops, 1 const, 4 2-input muxes, 2 6-input muxes, and 4 7-input muxes
          val nodeNames = {
            val ins = (0 until 4).map(in => s"$pfx.in$in")
            val regs = (0 until 4).flatMap { reg =>
              val regPfx = s"$pfx.reg$reg"
              Seq(s"$regPfx.in", s"$regPfx.reg", s"$regPfx.out")
            }
            val inMuxes = (0 until 4).flatMap { mux =>
              val muxPfx = s"$pfx.mux_bp$mux"
              (0 until 2).map(in => s"$muxPfx.in$in") ++ Seq(s"$muxPfx.mux", s"$muxPfx.out")
            }
            val opRegs = (0 until 2).flatMap { reg =>
              val regPfx = s"$pfx.reg_op$reg"
              Seq(s"$regPfx.in", s"$regPfx.reg", s"$regPfx.out")
            }
            val const = Seq(s"$pfx.const.const", s"$pfx.const.out")
            val alu = Seq(s"$pfx.alu.in_a", s"$pfx.alu.in_b",
              s"$pfx.alu.sel", s"$pfx.alu.out") ++ modes.map(_.op).zipWithIndex
                .map { case (op, ind) => s"$pfx.alu.fu_${op}" }
            val regRes = Seq(s"$pfx.reg_res.in", s"$pfx.reg_res.reg", s"$pfx.reg_res.out")
            val opMuxes = (0 until 2).flatMap { mux =>
              val muxPfx = s"$pfx.mux_reg_op${mux}_in"
              (0 until 6).map(in => s"$muxPfx.in$in") ++ Seq(s"$muxPfx.mux", s"$muxPfx.out")
            }
            val bpMuxes = (0 until 4).flatMap { mux =>
              val muxPfx = s"$pfx.mux_out$mux"
              (0 until 7).map(in => s"$muxPfx.in$in") ++ Seq(s"$muxPfx.mux", s"$muxPfx.out")
            }
            val outs = (0 until 4).map(out => s"$pfx.out$out")
            ins ++ regs ++ inMuxes ++ opRegs ++ const ++ alu ++ opMuxes ++ bpMuxes ++ regRes ++ outs
          }
          nodes.keys should (have size (nodeNames.length) and contain allElementsOf (nodeNames))
          
          /** Input nodes */
          (0 until 4).foreach { ind =>
            val inNode = nodes(s"$pfx.in$ind")
            val (regInNode, regRegNode, regOutNode) = {
              val regPfx = s"$pfx.reg$ind"
              (nodes(s"$regPfx.in"), nodes(s"$regPfx.reg"), nodes(s"$regPfx.out"))
            }
            val (muxIn0Node, muxIn1Node, muxMuxNode, muxOutNode) = {
              val muxPfx = s"$pfx.mux_bp$ind"
              (nodes(s"$muxPfx.in0"), nodes(s"$muxPfx.in1"), nodes(s"$muxPfx.mux"), nodes(s"$muxPfx.out"))
            }
            inNode.fanin  should have length 1
            inNode.fanout should have length 2
            inNode.fanout should (contain (regInNode) and contain (muxIn0Node))
            regInNode.fanin  should (have length 1 and contain (inNode))
            regInNode.fanout should (have length 1 and contain (regRegNode))
            regRegNode.fanin should (have length 1 and contain (regInNode))
            regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$pfx.reg$ind.out")))
            regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$pfx.reg$ind.reg")))
            regOutNode.fanout should (have length 1 and contain (muxIn1Node))
            muxIn0Node.fanin  should (have length 1 and contain (inNode))
            muxIn1Node.fanin  should (have length 1 and contain (regOutNode))
            muxIn0Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn1Node.fanout should (have length 1 and contain (muxMuxNode))
            muxMuxNode.fanin  should (have length 2 and contain (muxIn0Node) and contain (muxIn1Node))
            muxMuxNode.fanout should (have length 1 and contain (muxOutNode))
            muxOutNode.fanin  should (have length 1 and contain (muxMuxNode))
            muxOutNode.fanout should  contain allElementsOf (
              (0 until 2).map(op  => nodes(s"$pfx.mux_reg_op${op}_in.in$ind")) ++
              (0 until 4).map(out => nodes(s"$pfx.mux_out$out.in$ind"))
            )
          }
          
          /** Middle nodes */
          (0 until 2).foreach { op =>
            val muxPfx = s"$pfx.mux_reg_op${op}_in"
            val (muxMuxNode, muxOutNode) = (nodes(s"$muxPfx.mux"), nodes(s"$muxPfx.out"))
            (0 until 4).foreach { ind =>
              val muxInNode = nodes(s"$muxPfx.in$ind")
              muxInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.mux_bp$ind.out")))
              muxInNode.fanout should (have length 1 and contain (muxMuxNode))
            }
            val (muxIn4Node, muxIn5Node) = (nodes(s"$muxPfx.in4"), nodes(s"$muxPfx.in5"))
            muxIn4Node.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.out")))
            muxIn5Node.fanin  should (have length 1 and contain (nodes(s"$pfx.reg_res.out")))
            muxIn4Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn5Node.fanout should (have length 1 and contain (muxMuxNode))
            muxMuxNode.fanin  should  have length 6
            muxMuxNode.fanin  should  contain allElementsOf (
              (0 until 4).map(i => nodes(s"$muxPfx.in$i")) ++ Seq(muxIn4Node, muxIn5Node)
            )
            muxMuxNode.fanout should (have length 1 and contain (muxOutNode))
            muxOutNode.fanin  should (have length 1 and contain (muxMuxNode))
            muxOutNode.fanout should (have length 1 and contain (nodes(s"$pfx.reg_op$op.in")))
          }
          (0 until 2).foreach { ind =>
            val regPfx = s"$pfx.reg_op$ind"
            val (regInNode, regRegNode, regOutNode) = (
              nodes(s"$regPfx.in"), nodes(s"$regPfx.reg"), nodes(s"$regPfx.out")
            )
            regInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.mux_reg_op${ind}_in.out")))
            regInNode.fanout should (have length 1 and contain (regRegNode))
            regRegNode.fanin should (have length 1 and contain (regInNode))
            regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$regPfx.out")))
            regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$regPfx.reg")))
          }
          val (inANode, inBNode) = (nodes(s"$pfx.alu.in_a"), nodes(s"$pfx.alu.in_b"))
          nodes(s"$pfx.reg_op0.out").fanout should (have length 1 and contain (inANode))
          nodes(s"$pfx.reg_op1.out").fanout should (have length 1 and contain (inBNode))
          inANode.fanin  should (have length 1 and contain (nodes(s"$pfx.reg_op0.out")))
          inBNode.fanin  should (have length 1 and contain (nodes(s"$pfx.reg_op1.out")))
          inANode.fanout should  have length (modes.length)
          inBNode.fanout should  have length (modes.length)
          val selNode = nodes(s"$pfx.alu.sel")
          selNode.fanin should  have length (modes.length)
          modes.map(_.op).zipWithIndex.foreach { case (op, ind) =>
            val fuNode = nodes(s"$pfx.alu.fu_${op}")
            inANode.fanout should contain (fuNode)
            inBNode.fanout should contain (fuNode)
            fuNode.canMapOp(op) should be (true)
            fuNode.fanin  should (have length 2 and contain (inANode) and contain (inBNode))
            fuNode.fanout should (have length 1 and contain (selNode))
          }
          val (regInNode, regRegNode, regOutNode) = (
            nodes(s"$pfx.reg_res.in"), nodes(s"$pfx.reg_res.reg"), nodes(s"$pfx.reg_res.out")
          )
          val aluOutNode = nodes(s"$pfx.alu.out")
          selNode.fanout should (have length 1 and contain (aluOutNode))
          aluOutNode.fanin should (have length 1 and contain (selNode))
          aluOutNode.fanout should have length 7
          aluOutNode.fanout should contain allElementsOf (
            (0 until 2).map(i => nodes(s"$pfx.mux_reg_op${i}_in.in4")) ++ 
            (0 until 4).map(i => nodes(s"$pfx.mux_out$i.in4")) :+ 
            regInNode
          )
          regInNode.fanin should (have length 1 and contain (aluOutNode))
          regInNode.fanout should (have length 1 and contain (regRegNode))
          regRegNode.fanin should (have length 1 and contain (regInNode))
          regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$pfx.reg_res.out")))
          regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$pfx.reg_res.reg")))
          regOutNode.fanout should have length 6
          regOutNode.fanout should contain allElementsOf (
            (0 until 2).map(i => nodes(s"$pfx.mux_reg_op${i}_in.in5")) ++
            (0 until 4).map(i => nodes(s"$pfx.mux_out$i.in5"))
          )

          /** Output and crossbar nodes */
          (0 until 4).foreach { out =>
            val muxPfx = s"$pfx.mux_out$out"
            val (muxMuxNode, muxOutNode) = (nodes(s"$muxPfx.mux"), nodes(s"$muxPfx.out"))
            (0 until 4).foreach { ind =>
              val muxInNode = nodes(s"$muxPfx.in$ind")
              muxInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.mux_bp$ind.out")))
              muxInNode.fanout should (have length 1 and contain (muxMuxNode))
            }
            val (muxIn4Node, muxIn5Node, muxIn6Node) = (
              nodes(s"$muxPfx.in4"), nodes(s"$muxPfx.in5"), nodes(s"$muxPfx.in6")
            )
            muxIn4Node.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.out")))
            muxIn5Node.fanin  should (have length 1 and contain (nodes(s"$pfx.reg_res.out")))
            muxIn6Node.fanin  should (have length 1 and contain (nodes(s"$pfx.const.out")))
            muxIn4Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn5Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn6Node.fanout should (have length 1 and contain (muxMuxNode))
            muxMuxNode.fanin  should  have length 7
            muxMuxNode.fanin  should  contain allElementsOf (
              (0 until 4).map(i => nodes(s"$muxPfx.in$i")) ++ Seq(muxIn4Node, muxIn5Node, muxIn6Node)
            )
            muxMuxNode.fanout should (have length 1 and contain (muxOutNode))
            muxOutNode.fanin  should (have length 1 and contain (muxMuxNode))
            muxOutNode.fanout should (have length 1 and contain (nodes(s"$pfx.out$out")))
          }
        }
      }
    }
  }

  it should "convert a complex hierarchical architecture" in {
    // First, grab the example architecture, parse it, and convert it to a CGRA
    val cgra = CGRA(Architecture("src/test/resources/archparse/complex_hier_arch.xml"))

    // Test for failure for II = 0
    intercept[IllegalArgumentException] {
      MRRG(cgra, 0)
    }

    // Test for II = 1 .. 2
    (1 to 2).foreach { II =>
      val mrrg = MRRG(cgra, II)

      // Skip checking the RF - go straight to one of the PEs
      {
        val pfx = "block_0_1"
        val modes = cgra.subModules(pfx)
          .subModules("alu")
          .subModules("alu").asInstanceOf[AbstractFunctionUnit]
          .operations
        (0 until II).foreach { cycle =>
          val prevCycle = ((cycle - 1) % II) * (cycle - 1).sign.toInt
          val nextCycle = (cycle + 1) % II
          val nodes = mrrg.nodes(cycle).filter(_._1.startsWith(pfx))
          // Complex hierarchical architecture has no IIs or latencies either
          modes.forall(_.ii  == params.FUArgs("II").toInt) should be (true)
          modes.forall(_.lat == params.FUArgs("Latency").toInt) should be (true)
          // 4 ins and outs, 3 sub-modules
          val nodeNames = {
            val ins = (0 until 4).map(in => s"$pfx.in$in")
            // Bypass nodes
            val bpNodes = {
              val ins = (0 until 4).map(in => s"$pfx.bp.in$in")
              val regs = (0 until 4).flatMap { reg =>
                val regPfx = s"$pfx.bp.reg$reg"
                Seq(s"$regPfx.in", s"$regPfx.reg", s"$regPfx.out")
              }
              val muxes = (0 until 4).flatMap { mux =>
                val muxPfx = s"$pfx.bp.mux_out$mux"
                Seq(s"$muxPfx.in0", s"$muxPfx.in1", s"$muxPfx.mux", s"$muxPfx.out")
              }
              val outs = (0 until 4).map(out => s"$pfx.bp.out$out")
              ins ++ regs ++ muxes ++ outs
            }
            // Mux nodes
            val muxNodes = (0 until 2).flatMap { op =>
              (0 until 6).map(in => s"$pfx.mux_alu_op$op.in$in") ++ Seq(
                s"$pfx.mux_alu_op$op.mux", s"$pfx.mux_alu_op$op.out"
              )
            }
            // Const nodes
            val constNodes = Seq(s"$pfx.const.const", s"$pfx.const.out")
            // ALU nodes
            val aluNodes = {
              val ins = (0 until 2).map(in => s"$pfx.alu.op$in")
              val opRegs = (0 until 2).flatMap { op =>
                val regPfx = s"$pfx.alu.reg_op$op"
                Seq(s"$regPfx.in", s"$regPfx.reg", s"$regPfx.out")
              }
              val alu = Seq(s"$pfx.alu.alu.in_a", s"$pfx.alu.alu.in_b",
                s"$pfx.alu.alu.sel", s"$pfx.alu.alu.out") ++ modes.map(_.op).zipWithIndex
                  .map { case (op, ind) => s"$pfx.alu.alu.fu_${op}" }
              val resReg = Seq(s"$pfx.alu.reg_res.in", s"$pfx.alu.reg_res.reg", s"$pfx.alu.reg_res.out")
              val outs = Seq(s"$pfx.alu.alu_out", s"$pfx.alu.reg_out")
              ins ++ opRegs ++ alu ++ resReg ++ outs
            }
            // Crossbar nodes
            val xbNodes = {
              val ins = (0 until 7).map(in => s"$pfx.xb.in$in")
              val muxes = (0 until 4).flatMap { mux =>
                val muxPfx = s"$pfx.xb.mux_out$mux"
                (0 until 7).map(in => s"$muxPfx.in$in") ++ Seq(s"$muxPfx.mux", s"$muxPfx.out")
              }
              val outs = (0 until 4).map(out => s"$pfx.xb.out$out")
              ins ++ muxes ++ outs
            }
            val outs = (0 until 4).map(out => s"$pfx.out$out")
            ins ++ bpNodes ++ muxNodes ++ constNodes ++ aluNodes ++ xbNodes ++ outs
          }
          nodes.keys should (have size nodeNames.length and contain allElementsOf nodeNames)

          /** Input nodes */
          (0 until 4).foreach { ind =>
            val inNode   = nodes(s"$pfx.in$ind")
            val bpInNode = nodes(s"$pfx.bp.in$ind")
            inNode.fanin should have length 1
            inNode.fanout should (have length 1 and contain (bpInNode))
            // Bypass connections
            val (regInNode, regRegNode, regOutNode) = {
              val regPfx = s"$pfx.bp.reg$ind"
              (nodes(s"$regPfx.in"), nodes(s"$regPfx.reg"), nodes(s"$regPfx.out"))
            }
            val (muxIn0Node, muxIn1Node, muxMuxNode, muxOutNode) = {
              val muxPfx = s"$pfx.bp.mux_out$ind"
              (nodes(s"$muxPfx.in0"), nodes(s"$muxPfx.in1"), nodes(s"$muxPfx.mux"), nodes(s"$muxPfx.out"))
            }
            bpInNode.fanin should have length 1
            bpInNode.fanout should (have length 2 and contain (regInNode) and contain (muxIn0Node))
            regInNode.fanin should (have length 1 and contain (bpInNode))
            regInNode.fanout should (have length 1 and contain (regRegNode))
            regRegNode.fanin should (have length 1 and contain (regInNode))
            regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$pfx.bp.reg$ind.out")))
            regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$pfx.bp.reg$ind.reg")))
            regOutNode.fanout should (have length 1 and contain (muxIn1Node))
            muxIn0Node.fanin  should (have length 1 and contain (bpInNode))
            muxIn1Node.fanin  should (have length 1 and contain (regOutNode))
            muxIn0Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn1Node.fanout should (have length 1 and contain (muxMuxNode))
            muxMuxNode.fanin  should (have length 2 and contain (muxIn0Node) and contain (muxIn1Node))
            muxMuxNode.fanout should (have length 1 and contain (muxOutNode))
            muxOutNode.fanin  should (have length 1 and contain (muxMuxNode))
            val bpOutNode = nodes(s"$pfx.bp.out$ind")
            muxOutNode.fanout should (have length 1 and contain (bpOutNode))
            bpOutNode.fanin   should (have length 1 and contain (muxOutNode))
            bpOutNode.fanout  should (have length 3 and contain allElementsOf (0 until 2).map { op =>
              nodes(s"$pfx.mux_alu_op$op.in$ind")
            } :+ nodes(s"$pfx.xb.in$ind"))
          }

          /** Middle nodes */
          val (constNode, constOutNode) = (nodes(s"$pfx.const.const"), nodes(s"$pfx.const.out"))
          constNode.fanin     should  be (empty)
          constNode.fanout    should (have length 1 and contain (constOutNode))
          constOutNode.fanin  should (have length 1 and contain (constNode))
          constOutNode.fanout should (have length 1 and contain (nodes(s"$pfx.xb.in6")))
          (0 until 2).foreach { op =>
            // Multiplexer connections
            val muxPfx = s"$pfx.mux_alu_op$op"
            val (muxMuxNode, muxOutNode) = (nodes(s"$muxPfx.mux"), nodes(s"$muxPfx.out"))
            (0 until 4).foreach { ind =>
              val muxInNode = nodes(s"$muxPfx.in$ind")
              muxInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.bp.out$ind")))
              muxInNode.fanout should (have length 1 and contain (muxMuxNode))
            }
            val (muxIn4Node, muxIn5Node) = (nodes(s"$muxPfx.in4"), nodes(s"$muxPfx.in5"))
            muxIn4Node.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.alu_out")))
            muxIn5Node.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.reg_out")))
            muxIn4Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn5Node.fanout should (have length 1 and contain (muxMuxNode))
            muxMuxNode.fanin  should (have length 6 and contain allElementsOf (
              (0 until 4).map(in => nodes(s"$muxPfx.in$in")) ++ Seq(muxIn4Node, muxIn5Node)
            ))
            muxMuxNode.fanout should (have length 1 and contain (muxOutNode))
            muxOutNode.fanin  should (have length 1 and contain (muxMuxNode))
            muxOutNode.fanout should (have length 1 and contain (nodes(s"$pfx.alu.op$op")))
            // Register connections
            val regPfx = s"$pfx.alu.reg_op$op"
            val (regInNode, regRegNode, regOutNode) = (
              nodes(s"$regPfx.in"), nodes(s"$regPfx.reg"), nodes(s"$regPfx.out")
            )
            regInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.op$op")))
            regInNode.fanout should (have length 1 and contain (regRegNode))
            regRegNode.fanin should (have length 1 and contain (regInNode))
            regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$regPfx.out")))
            regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$regPfx.reg")))
          }
          // ALU connections
          val (inANode, inBNode) = (nodes(s"$pfx.alu.alu.in_a"), nodes(s"$pfx.alu.alu.in_b"))
          nodes(s"$pfx.alu.reg_op0.out").fanout should (have length 1 and contain (inANode))
          nodes(s"$pfx.alu.reg_op1.out").fanout should (have length 1 and contain (inBNode))
          inANode.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.reg_op0.out")))
          inBNode.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.reg_op1.out")))
          inANode.fanout should  have length (modes.size)
          inBNode.fanout should  have length (modes.size)
          val selNode = nodes(s"$pfx.alu.alu.sel")
          selNode.fanin should  have length (modes.length)
          modes.map(_.op).zipWithIndex.foreach { case (op, ind) =>
            val fuNode = nodes(s"$pfx.alu.alu.fu_${op}")
            inANode.fanout should contain (fuNode)
            inBNode.fanout should contain (fuNode)
            fuNode.canMapOp(op) should be (true)
            fuNode.fanin should (have length 2 and contain (inANode) and contain (inBNode))
            fuNode.fanout    should (have length 1 and contain (selNode))
          }
          val (regInNode, regRegNode, regOutNode) = (
            nodes(s"$pfx.alu.reg_res.in"), nodes(s"$pfx.alu.reg_res.reg"), nodes(s"$pfx.alu.reg_res.out")
          )
          val aluOutNode = nodes(s"$pfx.alu.alu.out")
          selNode.fanout    should (have length 1 and contain (aluOutNode))
          aluOutNode.fanin  should (have length 1 and contain (selNode))
          aluOutNode.fanout should (have length 2 and contain allElementsOf (
            Seq(nodes(s"$pfx.alu.alu_out"), regInNode)
          ))
          regInNode.fanin  should (have length 1 and contain (aluOutNode))
          regInNode.fanout should (have length 1 and contain (regRegNode))
          regRegNode.fanin should (have length 1 and contain (regInNode))
          regRegNode.fanout should (have length 1 and contain (mrrg.nodes(nextCycle)(s"$pfx.alu.reg_res.out")))
          regOutNode.fanin should (have length 1 and contain (mrrg.nodes(prevCycle)(s"$pfx.alu.reg_res.reg")))
          regOutNode.fanout should (have length 1 and contain (nodes(s"$pfx.alu.reg_out")))
          nodes(s"$pfx.alu.alu_out").fanout should (have length 3 and contain allElementsOf (
            Seq(nodes(s"$pfx.mux_alu_op0.in4"), nodes(s"$pfx.mux_alu_op1.in4"), nodes(s"$pfx.xb.in4"))
          ))
          nodes(s"$pfx.alu.reg_out").fanout should (have length 3 and contain allElementsOf (
            Seq(nodes(s"$pfx.mux_alu_op0.in5"), nodes(s"$pfx.mux_alu_op1.in5"), nodes(s"$pfx.xb.in5"))
          ))

          /** Output and crossbar nodes */
          (0 until 4).foreach { ind =>
            val xbInNode = nodes(s"$pfx.xb.in$ind")
            xbInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.bp.out$ind")))
            xbInNode.fanout should (have length 4 and contain allElementsOf (
              (0 until 4).map { mux => nodes(s"$pfx.xb.mux_out$mux.in$ind") }
            ))
          }
          val (xbIn4Node, xbIn5Node, xbIn6Node) = (
            nodes(s"$pfx.xb.in4"), nodes(s"$pfx.xb.in5"), nodes(s"$pfx.xb.in6")
          )
          xbIn4Node.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.alu_out")))
          xbIn5Node.fanin  should (have length 1 and contain (nodes(s"$pfx.alu.reg_out")))
          xbIn6Node.fanin  should (have length 1 and contain (constOutNode))
          xbIn4Node.fanout should (have length 4 and contain allElementsOf (
            (0 until 4).map(mux => nodes(s"$pfx.xb.mux_out$mux.in4"))
          ))
          xbIn5Node.fanout should (have length 4 and contain allElementsOf (
            (0 until 4).map(mux => nodes(s"$pfx.xb.mux_out$mux.in5"))
          ))
          xbIn6Node.fanout should (have length 4 and contain allElementsOf (
            (0 until 4).map(mux => nodes(s"$pfx.xb.mux_out$mux.in6"))
          ))
          (0 until 4).foreach { out =>
            val muxPfx = s"$pfx.xb.mux_out$out"
            val (muxMuxNode, muxOutNode) = (nodes(s"$muxPfx.mux"), nodes(s"$muxPfx.out"))
            (0 until 4).foreach { ind =>
              val muxInNode = nodes(s"$muxPfx.in$ind")
              muxInNode.fanin  should (have length 1 and contain (nodes(s"$pfx.xb.in$ind")))
              muxInNode.fanout should (have length 1 and contain (muxMuxNode))
            }
            val (muxIn4Node, muxIn5Node, muxIn6Node) = (
              nodes(s"$muxPfx.in4"), nodes(s"$muxPfx.in5"), nodes(s"$muxPfx.in6")
            )
            muxIn4Node.fanin  should (have length 1 and contain (nodes(s"$pfx.xb.in4")))
            muxIn5Node.fanin  should (have length 1 and contain (nodes(s"$pfx.xb.in5")))
            muxIn6Node.fanin  should (have length 1 and contain (nodes(s"$pfx.xb.in6")))
            muxIn4Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn5Node.fanout should (have length 1 and contain (muxMuxNode))
            muxIn6Node.fanout should (have length 1 and contain (muxMuxNode))
            muxMuxNode.fanin  should (have length 7 and contain allElementsOf (
              (0 until 4).map(in => nodes(s"$muxPfx.in$in")) ++ Seq(muxIn4Node, muxIn5Node, muxIn6Node)
            ))
            muxMuxNode.fanout should (have length 1 and contain (muxOutNode))
            muxOutNode.fanin  should (have length 1 and contain (muxMuxNode))
            val xbOutNode = nodes(s"$pfx.xb.out$out")
            muxOutNode.fanout should (have length 1 and contain (xbOutNode))
            xbOutNode.fanin   should (have length 1 and contain (muxOutNode))
            val outNode   = nodes(s"$pfx.out$out")
            xbOutNode.fanout should (have length 1 and contain (outNode))
            outNode.fanin    should (have length 1 and contain (xbOutNode))
          }
        }
      }
    }
  }
}
