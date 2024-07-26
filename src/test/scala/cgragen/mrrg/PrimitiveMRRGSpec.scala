package cgragen.mrrg

import scala.collection.mutable

import cgragen.mrrg.MRRG

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class PrimitiveMRRGSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Primitive-to-MRRG converter"

  /** Test all of the primitives for having the right nodes and the right 
   * edges. For sequential elements, we only check IIs of 1 and 2 as the 
   * interesting differences between them are highlighted by these values.
   */

  it should "generate the MRRG of a constant unit" in {
    import cgragen.cgra.primitives.AbstractConstantUnit

    // Create a new constant unit primitive
    val const = AbstractConstantUnit("const")

    // Test for failure for II = 0
    intercept[IllegalArgumentException] { MRRG(const, 0) }

    // Test for II = 1 .. 2
    (1 to 2).foreach { II =>
      val mrrg = MRRG(const, II)
      mrrg.nodes should have size (II)
      (0 until II).foreach { cycle =>
        val nodes = mrrg.nodes(cycle)
        nodes should have size 2 // 1 const and 1 out
        nodes.keys should (contain ("const") and contain ("out"))

        // As these checks are exhaustive, there can be no unexpected 
        // inter-cycle edges - thus, no need to check
        /** Middle node */
        nodes("const").fanin  should  be (empty)
        nodes("const").fanout should (have size 1 and contain (nodes("out")))

        /** Output node */
        nodes("out").fanin    should (have size 1 and contain (nodes("const")))
        nodes("out").fanout   should  be (empty)
      }
    }
  }

  it should "generate the MRRG of a function unit" in {
    import cgragen.cgra.primitives.AbstractFunctionUnit
    import cgragen.dfgparse.Opcode

    // Test various numbers of operations
    val possibleOps = (Opcode.values -- Seq(Opcode.OpInput, Opcode.OpOutput, Opcode.OpUnspecified, Opcode.OpConst)).toList
    (1 until possibleOps.size by possibleOps.size / 4).foreach { n =>
      // Select n operations to include
      val ops = scala.util.Random.shuffle(possibleOps).take(n)

      // Test for failure on invalid II or latency
      intercept[Exception] {
        val modes = ops.map(op => (op, 0, 0))
        AbstractFunctionUnit("fu", modes.map(_._1), modes.map(_._2), modes.map(_._3))
      }
      intercept[Exception] {
        val modes = ops.map(op => (op, 1, -1))
        AbstractFunctionUnit("fu", modes.map(_._1), modes.map(_._2), modes.map(_._3))
      }

      // Test for failure on operation II or latency beyond MRRG II
      (1 to 2).foreach { ii =>
        intercept[MRRGGenerationException] {
          val modes = ops.map(op => (op, ii + 1, 0))
          MRRG(AbstractFunctionUnit("fu", modes.map(_._1), modes.map(_._2), modes.map(_._3)), ii)
        }
        intercept[MRRGGenerationException] {
          val modes = ops.map(op => (op, 1, ii))
          MRRG(AbstractFunctionUnit("fu", modes.map(_._1), modes.map(_._2), modes.map(_._3)), ii)
        }
      }

      // Test for failure for II = 0
      intercept[IllegalArgumentException] {
        val modes = ops.map(op => (op, 1, 0))
        MRRG(AbstractFunctionUnit("fu", modes.map(_._1), modes.map(_._2), modes.map(_._3)), 0)
      }

      // Test for II = 1 .. 5
      val rng = new scala.util.Random(42)
      (1 to 5).foreach { II =>
        val modes = ops.map(op => (op, rng.nextInt(II) + 1, rng.nextInt(II)))
        val fu    = AbstractFunctionUnit("fu", modes.map(_._1), modes.map(_._2), modes.map(_._3))
        val mrrg  = MRRG(fu, II)

        mrrg.nodes should have size (II)
        (0 until II).foreach { cycle =>
          val nodes        = mrrg.nodes(cycle)
          val modesInCycle = modes.filter { case (_, opII, _) => (cycle % opII) == 0 }
          // 2 inputs, n select inputs, 1 select, 1 out, and modesInCycle.size ops
          nodes.keys should have size (4 + modesInCycle.size)
          nodes.keys should (contain ("in_a") and contain ("in_b"))
          modesInCycle.foreach { case (op, _, _) => 
            nodes.keys should contain (s"fu_${op}")
          }
          nodes.keys should (contain ("out") and contain ("sel"))

          /** Input nodes and some middle nodes */
          nodes("in_a").fanin should be (empty)
          nodes("in_b").fanin should be (empty)
          nodes("in_a").fanout should have size (modesInCycle.size)
          nodes("in_b").fanout should have size (modesInCycle.size)
          modesInCycle.foreach { case (op, _, opLatency) =>
            val fuNode = nodes(s"fu_${op}")
            fuNode.canMapOp(op) should be (true)
            nodes("in_a").fanout should contain (fuNode)
            nodes("in_b").fanout should contain (fuNode)
            fuNode.fanin should have size 2
            fuNode.fanin should (contain (nodes("in_a")) and contain (nodes("in_b")))
            val selNode = mrrg.nodes((cycle + opLatency) % II)("sel")
            fuNode.fanout should (have size 1 and contain (selNode))
            selNode.fanin should contain (fuNode)
          }

          /** ... and the remaining middle nodes */
          nodes("sel").fanout should (have size 1 and contain (nodes("out")))

          /** Output nodes */
          nodes("out").fanin  should (have size 1 and contain (nodes("sel")))
          nodes("out").fanout should  be (empty)
        }
      }
    }
  }

  it should "generate the MRRG of a multiplexer" in {
    import cgragen.cgra.primitives.AbstractMultiplexerUnit

    // Test various multiplexer sizes
    (1 to 4).foreach { lg2MuxSize =>
      // Create a new multiplexer primitive
      val muxSize = 1 << lg2MuxSize
      val mux = AbstractMultiplexerUnit("mux", muxSize)
      
      // Test for failure for II = 0
      intercept[IllegalArgumentException] { MRRG(mux, 0) }

      // Test for II = 1 .. 2
      (1 to 2).foreach { II =>
        val mrrg = MRRG(mux, II)
        mrrg.nodes should have size (II)
        (0 until II).foreach { cycle =>
          val nodes = mrrg.nodes(cycle)
          nodes should have size (muxSize + 2) // muxSize ins, 1 mux, and 1 out
          (0 until muxSize).foreach(i => nodes.keys should contain (s"in$i"))
          nodes.keys should (contain ("mux") and contain ("out"))

          // Like above, these checks are exhaustive and no unexpected 
          // inter-cycle connections can exist
          /** Input nodes */
          (0 until muxSize).foreach { i =>
            nodes(s"in$i").fanin  should  be (empty)
            nodes(s"in$i").fanout should (have size 1 and contain (nodes("mux")))
          }

          /** Middle node */
          nodes("mux").fanin  should  have size (muxSize)
          nodes("mux").fanin  should contain only ((0 until muxSize).map(i => nodes(s"in$i")):_*)
          nodes("mux").fanout should (have size 1 and contain (nodes("out")))
          
          /** Output node */
          nodes("out").fanin  should (have size 1 and contain (nodes("mux")))
          nodes("out").fanout should  be (empty)
        }
      }
    }
  }

  it should "generate the MRRG of a register" in {
    import cgragen.cgra.primitives.AbstractRegisterUnit

    // Create a new register primitive
    val reg = AbstractRegisterUnit("reg")

    // Test for failure for II = 0
    intercept[IllegalArgumentException] { MRRG(reg, 0) }

    // Test for II = 1 .. 2
    (1 to 2).foreach { II =>
      val mrrg = MRRG(reg, II)
      mrrg.nodes should have size II
      (0 until II).foreach { cycle =>
        val prevCycle = ((cycle - 1) % II) * (cycle - 1).sign.toInt
        val nextCycle = (cycle + 1) % II
        val nodes = mrrg.nodes(cycle)
        nodes should have size 3 // 1 in, 1 reg, and 1 out
        nodes.keys should (contain ("in") and contain ("reg") and contain ("out"))

        /** Input node */
        nodes("in").fanin  should  be (empty)
        nodes("in").fanout should (have size 1 and contain (nodes("reg")))

        /** Middle node */
        nodes("reg").fanin should (have size 1 and contain (nodes("in")))
        nodes("reg").fanout should (have size 1 and contain (mrrg.nodes(nextCycle)("out")))

        /** Output node */
        nodes("out").fanin should (have size 1 and contain (mrrg.nodes(prevCycle)("reg")))
        nodes("out").fanout should  be (empty)
      }
    }
  }

  it should "generate the MRRG of a register file" in {
    import cgragen.cgra.primitives.AbstractRegisterFileUnit

    // Test various numbers of inputs, outputs and registers
    (1 to 4).foreach { numInputs =>
      (1 to 4).foreach { numOutputs =>
        (1 to 3).foreach { numRegsLg2 =>
          // Create a new register file primitive
          val rf = AbstractRegisterFileUnit("rf", numInputs, numOutputs, numRegsLg2)
          val numRegs = 1 << numRegsLg2

          // Test for failure for II = 0
          intercept[IllegalArgumentException] { MRRG(rf, 0) }

          // Test for II = 1 .. 2
          (1 to 2).foreach { II =>
            val mrrg = MRRG(rf, II)

            mrrg.nodes should have size II
            (0 until II).foreach { cycle =>
              val prevCycle = ((cycle - 1) % II) * (cycle - 1).sign.toInt
              val nextCycle =  (cycle + 1) % II
              val nodes = mrrg.nodes(cycle)
              val nodeNames = {
                val ins  = (0 until numInputs).map(i => s"in$i")
                val regSels = (0 until numRegs).flatMap { reg =>
                  (0 until numInputs).map(in => s"reg${reg}_sel$in")
                }
                val regs = (0 until numRegs).map(reg => s"reg$reg")
                val outSels = (0 until numOutputs).flatMap { out =>
                  (0 until numRegs).map(reg => s"out${out}_sel$reg")
                }
                val outs = (0 until numOutputs).map(out => s"out$out")
                ins ++ regSels ++ regs ++ outSels ++ outs
              }
              nodes.keys should (have size (nodeNames.size) and contain allElementsOf nodeNames)

              /** Input nodes */
              (0 until numInputs).foreach { input =>
                val inNode = nodes(s"in$input")
                inNode.fanin  should be (empty)
                inNode.fanout should have size (numRegs)
                inNode.fanout should contain allElementsOf ((0 until numRegs).map { reg =>
                  nodes(s"reg${reg}_sel$input")
                })
              }

              /** Middle and register select nodes */
              (0 until numRegs).foreach { register =>
                val regNode = nodes(s"reg$register")
                if (II != 1) regNode.fanin should have size (numInputs + 1)
                else         regNode.fanin should have size (numInputs)
                (0 until numInputs).foreach { input =>
                  val regSelNode = nodes(s"reg${register}_sel$input")
                  regSelNode.fanin  should (have size 1 and contain (nodes(s"in$input")))
                  regSelNode.fanout should (have size 1 and contain (regNode))
                  regNode.fanin     should  contain (regSelNode)
                }
                if (II != 1) {
                  regNode.fanin  should contain (mrrg.nodes(prevCycle)(s"reg$register"))
                  regNode.fanout should contain (mrrg.nodes(nextCycle)(s"reg${register}"))
                }
                regNode.fanout should contain allElementsOf ((0 until numOutputs).map { out =>
                  mrrg.nodes(nextCycle)(s"out${out}_sel$register")
                })
              }

              /** Output and output select nodes */
              (0 until numOutputs).foreach { output =>
                val outNode = nodes(s"out$output")
                outNode.fanin should have size (numRegs)
                (0 until numRegs).foreach { register =>
                  val outSelNode = nodes(s"out${output}_sel$register")
                  outSelNode.fanin  should (have size 1 and contain (mrrg.nodes(prevCycle)(s"reg$register")))
                  outSelNode.fanout should (have size 1 and contain (outNode))
                  outNode.fanin     should  contain (outSelNode)
                }
                outNode.fanout should be (empty)
              }
            }
          }
        }
      }
    }
  }
}
