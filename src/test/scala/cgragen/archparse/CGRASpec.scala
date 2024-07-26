package cgragen.archparse

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CGRASpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Architecture parser"

  it should "parse a simple architecture" in {
    // Grab the simple example architecture file first
    val simpleArch = Architecture("src/test/resources/archparse/simple_arch.xml")
    
    // Then validate that it contains the expected content
    simpleArch.modTemplates.map(_._1) should (have size 1 and contain ("pe"))

    // ... first, check that the PE module template exists and has the right content
    val peTemplate = simpleArch.modTemplates("pe")
    peTemplate.wires should be (empty)
    peTemplate.subModules should be (empty)
    val pePorts = (0 until 4).map { i => s"in$i" } :+ "out"
    peTemplate.ports.keys should (have size (pePorts.length) and contain allElementsOf pePorts)

    val prims = Seq("rgstr", "func", "const", "mux_func_in_a", "mux_func_in_b", "mux_rgstr_in")
    peTemplate.primitives should have length prims.length // reg, func, const and three muxes
    peTemplate.primitives.map(_._2("name")) should contain allElementsOf (prims)

    val peConns = {
      val muxedIns = Seq("mux_func_in_a", "mux_func_in_b").flatMap { mux =>
        (0 until 4).map { i =>
          (s"this.in$i", s"$mux.in$i")
        }
      }
      val muxedOuts = Seq(("mux_func_in_a.out", "func.in_a"), ("mux_func_in_b.out", "func.in_b"))
      val funcOuts = Seq(("const.out", "mux_rgstr_in.in0"), ("func.out", "mux_rgstr_in.in1"))
      val givens = Seq(("mux_rgstr_in.out", "rgstr.in"), ("rgstr.out", "this.out"))
      muxedIns ++ muxedOuts ++ funcOuts ++ givens
    }
    peTemplate.connections should have length peConns.length
    peTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf peConns

    // Then validate that the right modules have been instantiated
    val archMods = (0 until 2).flatMap { row =>
      (0 until 2).map { col =>
        s"block_${row}_${col}"
      }
    }
    simpleArch.subModules should have size archMods.size
    simpleArch.subModules.map(_._1) should contain allElementsOf archMods

    // ... and that they are correctly connected
    val archConns = {
      val verts = (0 until 2).flatMap { col =>
        Seq(
          (s"block_0_${col}.out", s"block_1_${col}.in0"),
          (s"block_1_${col}.out", s"block_0_${col}.in2")
        )
      }
      val horis = (0 until 2).flatMap { row =>
        Seq(
          (s"block_${row}_0.out", s"block_${row}_1.in3"),
          (s"block_${row}_1.out", s"block_${row}_0.in1")
        )
      }
      verts ++ horis
    }
    simpleArch.connections should have length archConns.size
    simpleArch.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf archConns
  }

  it should "parse a complex architecture" in {
    // Grab the complex example architecture file first
    val complexArch = Architecture("src/test/resources/archparse/complex_arch.xml")

    // Then validate that it contains the expected content
    complexArch.modTemplates should have size 2 // hycubePE and globalRF
    complexArch.modTemplates.keys should (contain ("hycubePE") and contain ("globalRF"))

    // ... first check that the HyCUBE-inspired PE module template exists and has the right content
    val peTemplate = complexArch.modTemplates("hycubePE")
    peTemplate.wires should be (empty)
    peTemplate.subModules should be (empty)
    val pePorts = (0 until 4).flatMap { i =>
      Seq(s"in$i", s"out$i")
    }
    peTemplate.ports.keys should (have size pePorts.length and contain allElementsOf pePorts)

    val pePrims = {
      val pipeRegs = (0 until 4).map(i => s"reg$i")
      val opRegs   = (0 until 2).map(i => s"reg_op$i")
      val bpMuxes  = (0 until 4).map(i => s"mux_bp$i")
      val opMuxes  = (0 until 2).map(i => s"mux_reg_op${i}_in")
      val outMuxes = (0 until 4).map(i => s"mux_out$i")
      pipeRegs ++ opRegs ++ bpMuxes ++ opMuxes ++ outMuxes ++ Seq("const", "alu", "reg_res")
    }
    peTemplate.primitives should have length pePrims.length
    peTemplate.primitives.map(_._2("name")) should contain allElementsOf pePrims

    val peConns = {
      // wires are replaced by their respective multiplexer's output
      val pipeConns  = (0 until 4).map { i => (s"this.in$i", s"reg$i.in") }
      val bypassIns  = (0 until 4).flatMap { i =>
        Seq(
          (s"this.in$i", s"mux_bp$i.in0"),
          (s"reg$i.out", s"mux_bp$i.in1")
        )
      }
      val operandIns = (0 until 2).flatMap { i =>
        // Generate multiplexer to multiplexer connections, and manually add irregular ones
        (0 until 4).map { j =>
          (s"mux_bp$j.out", s"mux_reg_op${i}_in.in$j")
        } ++ Seq(
          ("alu.out", s"mux_reg_op${i}_in.in4"), 
          ("reg_res.out", s"mux_reg_op${i}_in.in5"), 
          (s"mux_reg_op${i}_in.out", s"reg_op$i.in")
        )
      }
      val crossOuts  = (0 until 4).flatMap { i =>
        // Generate multiplexer to multiplexer connections, and manually add irregular ones
        (0 until 4).map { j =>
          (s"mux_bp$j.out", s"mux_out$i.in$j")
        } ++ Seq(
          ("alu.out", s"mux_out$i.in4"), 
          ("reg_res.out", s"mux_out$i.in5"), 
          ("const.out", s"mux_out$i.in6"), 
          (s"mux_out$i.out", s"this.out$i")
        )
      }
      val givens = Seq(
        ("reg_op0.out", "alu.in_a"), 
        ("reg_op1.out", "alu.in_b"),
        ("alu.out", "reg_res.in")
      )
      pipeConns ++ bypassIns ++ operandIns ++ crossOuts ++ givens
    }
    peTemplate.connections should have length peConns.length
    peTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf peConns

    // ... next check that the global register file module template exists and has the right content
    val rfTemplate = complexArch.modTemplates("globalRF")
    rfTemplate.wires should be (empty)
    rfTemplate.subModules should be (empty)
    val rfPorts = (0 until 4).flatMap { i =>
      Seq(s"in$i", s"out$i")
    }
    rfTemplate.ports.keys should (have size (rfPorts.length) and contain allElementsOf rfPorts)

    rfTemplate.primitives should have length 1 // only a reg. file
    rfTemplate.primitives.head._2("name") should be ("rf")
    
    val rfConns = (0 until 4).flatMap { i =>
      Seq(
        (s"this.in$i", s"rf.in$i"),
        (s"rf.out$i", s"this.out$i")
      )
    }
    rfTemplate.connections should have length rfConns.length
    rfTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf rfConns

    // Then validate that the right modules have been instantiated
    val archMods = ((0 until 4).flatMap { row =>
      (1 until 5).map { col =>
        s"block_${row}_${col}"
      }
    } :+ "block_0_0")
    complexArch.subModules should have size archMods.length
    complexArch.subModules.map(_._1) should contain allElementsOf archMods

    // ... and that they are correctly connected
    val archConns = {
      val rfConns = (0 until 4).flatMap { i =>
        Seq(
          (s"block_${i}_1.out3", s"block_0_0.in$i"),
          (s"block_0_0.out$i", s"block_${i}_1.in3")
        )
      }
      val verts = (0 until 4).flatMap { row =>
        (1 until 5).flatMap { col =>
          Seq(
            (s"block_${row}_${col}.out2", s"block_${(row+1)%4}_${col}.in0"),
            (s"block_${(row+1)%4}_${col}.out0", s"block_${row}_${col}.in2")
          )
        }
      }
      val horis = (0 until 4).flatMap { row =>
        (1 until 4).flatMap { col =>
          Seq(
            (s"block_${row}_${col}.out1", s"block_${row}_${col+1}.in3"),
            (s"block_${row}_${col+1}.out3", s"block_${row}_${col}.in1")
          )
        }
      }
      rfConns ++ verts ++ horis
    }
    complexArch.connections should have length archConns.length
    complexArch.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf archConns
  }

  it should "parse a complex hierarchical architecture" in {
    // Grab the complex example architecture file first
    val hierArch = Architecture("src/test/resources/archparse/complex_hier_arch.xml")

    // Then validate that it contains the expected content
    val templates = Seq("hycubePE", "hycube_bypass", "hycube_alu", "hycube_crossbar", "globalRF")
    hierArch.modTemplates should have size templates.length // hycubePE and its sub-modules, and globalRF
    hierArch.modTemplates.keys should contain allElementsOf templates

    // ... first check that the HyCUBE-inspired PE module template exists and has the right content
    val peTemplate = hierArch.modTemplates("hycubePE")
    peTemplate.wires should be (empty)
    val pePorts = (0 until 4).flatMap(i => Seq(s"in$i", s"out$i"))
    peTemplate.ports.keys should (have size pePorts.length and contain allElementsOf pePorts)

    val pePrims = (0 until 2).map(i => s"mux_alu_op$i") :+ "const"
    peTemplate.primitives should have length pePrims.length
    peTemplate.primitives.map(_._2("name")) should contain allElementsOf pePrims

    val peConns = {
      val bypassIns = (0 until 4).map(i => (s"this.in$i", s"bp.in$i"))
      val xbIns     = (0 until 4).map(i => (s"bp.out$i", s"xb.in$i")) ++ Seq(
        ("alu.alu_out", "xb.in4"), ("alu.reg_out", "xb.in5"), ("const.out", "xb.in6")
      )
      val operandIns = (0 until 2).flatMap { i =>
        // Generate multiplexer connections
        (0 until 4).map { j =>
          (s"bp.out$j", s"mux_alu_op${i}.in$j")
        } ++ Seq(
          ("alu.alu_out", s"mux_alu_op${i}.in4"), 
          ("alu.reg_out", s"mux_alu_op${i}.in5"), 
          (s"mux_alu_op${i}.out", s"alu.op$i")
        )
      }
      val xbOuts = (0 until 4).map(i => (s"xb.out$i", s"this.out$i"))
      bypassIns ++ xbIns ++ operandIns ++ xbOuts
    }
    peTemplate.connections should have length peConns.length
    peTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf peConns

    // ... check that the PE module has the expected sub-modules
    val peSubMods = Seq("bp", "alu", "xb")
    peTemplate.subModules.map(_._1) should (have size peSubMods.length and contain allElementsOf peSubMods)

    val bypassTemplate = hierArch.modTemplates("hycube_bypass")
    bypassTemplate.wires should be (empty)
    val bypassPorts = (0 until 4).flatMap(i => Seq(s"in$i", s"out$i"))
    bypassTemplate.ports.keys should (have size bypassPorts.length and contain allElementsOf bypassPorts)

    val bypassPrims = {
      val regs  = (0 until 4).map(i => s"reg$i")
      val muxes = (0 until 4).map(i => s"mux_out$i")
      regs ++ muxes
    }
    bypassTemplate.primitives should have length bypassPrims.length
    bypassTemplate.primitives.map(_._2("name")) should contain allElementsOf bypassPrims

    val bypassConns = {
      val regIns    = (0 until 4).map(i => (s"this.in$i", s"reg$i.in"))
      val muxedOuts = (0 until 4).flatMap { i =>
        Seq(
          (s"this.in$i", s"mux_out$i.in0"), 
          (s"reg$i.out", s"mux_out$i.in1"), 
          (s"mux_out$i.out", s"this.out$i")
        )
      }
      regIns ++ muxedOuts
    }
    bypassTemplate.connections should have length bypassConns.length
    bypassTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf bypassConns

    val aluTemplate = hierArch.modTemplates("hycube_alu")
    aluTemplate.wires should be (empty)
    val aluPorts = Seq("op0", "op1", "alu_out", "reg_out")
    aluTemplate.ports.keys should (have size aluPorts.length and contain allElementsOf aluPorts)

    val aluPrims = {
      val opRegs = (0 until 2).map(i => s"reg_op$i")
      opRegs ++ Seq("reg_res", "alu")
    }
    aluTemplate.primitives should have length aluPrims.length
    aluTemplate.primitives.map(_._2("name")) should contain allElementsOf aluPrims

    val aluConns = {
      val regIns  = (0 until 2).map(i => (s"this.op$i", s"reg_op$i.in"))
      val aluIns  = Seq(("reg_op0.out", "alu.in_a"), ("reg_op1.out", "alu.in_b"))
      val aluOuts = Seq(("alu.out", "reg_res.in"), ("alu.out", "this.alu_out"))
      regIns ++ aluIns ++ aluOuts :+ ("reg_res.out", "this.reg_out")
    }
    aluTemplate.connections should have length aluConns.length
    aluTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf aluConns

    val xbTemplate = hierArch.modTemplates("hycube_crossbar")
    xbTemplate.wires should be (empty)
    val xbPorts = {
      val ins  = (0 until 7).map(i => s"in$i")
      val outs = (0 until 4).map(i => s"out$i")
      ins ++ outs
    }
    xbTemplate.ports.keys should (have size xbPorts.length and contain allElementsOf xbPorts)

    val xbPrims = (0 until 4).map(i => s"mux_out$i")
    xbTemplate.primitives should have length xbPrims.length
    xbTemplate.primitives.map(_._2("name")) should contain allElementsOf xbPrims
    
    val xbConns = (0 until 4).flatMap { i =>
      // Generate multiplexer connections
      (0 until 7).map(j => (s"this.in$j", s"mux_out$i.in$j")) :+ (s"mux_out$i.out", s"this.out$i")
    }
    xbTemplate.connections should have length xbConns.length
    xbTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf xbConns

    // ... next check that the global register file module template exists and has the right content
    val rfTemplate = hierArch.modTemplates("globalRF")
    rfTemplate.wires should be (empty)
    rfTemplate.subModules should be (empty)
    val rfPorts = (0 until 4).flatMap { i =>
      Seq(s"in$i", s"out$i")
    }
    rfTemplate.ports.keys should (have size (rfPorts.length) and contain allElementsOf rfPorts)

    rfTemplate.primitives should have length 1 // only a reg. file
    rfTemplate.primitives.head._2("name") should be ("rf")
    
    val rfConns = (0 until 4).flatMap { i =>
      Seq(
        (s"this.in$i", s"rf.in$i"),
        (s"rf.out$i", s"this.out$i")
      )
    }
    rfTemplate.connections should have length rfConns.length
    rfTemplate.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf rfConns

    // Then validate that the right modules have been instantiated
    val archMods = ((0 until 4).flatMap { row =>
      (1 until 5).map { col =>
        s"block_${row}_${col}"
      }
    } :+ "block_0_0")
    hierArch.subModules should have size archMods.length
    hierArch.subModules.map(_._1) should contain allElementsOf archMods

    // ... and that they are correctly connected
    val archConns = {
      val rfConns = (0 until 4).flatMap { i =>
        Seq(
          (s"block_${i}_1.out3", s"block_0_0.in$i"),
          (s"block_0_0.out$i", s"block_${i}_1.in3")
        )
      }
      val verts = (0 until 4).flatMap { row =>
        (1 until 5).flatMap { col =>
          Seq(
            (s"block_${row}_${col}.out2", s"block_${(row+1)%4}_${col}.in0"),
            (s"block_${(row+1)%4}_${col}.out0", s"block_${row}_${col}.in2")
          )
        }
      }
      val horis = (0 until 4).flatMap { row =>
        (1 until 4).flatMap { col =>
          Seq(
            (s"block_${row}_${col}.out1", s"block_${row}_${col+1}.in3"),
            (s"block_${row}_${col+1}.out3", s"block_${row}_${col}.in1")
          )
        }
      }
      rfConns ++ verts ++ horis
    }
    hierArch.connections should have length archConns.length
    hierArch.connections.map(c => (c.fromPorts, c.toPorts)) should contain allElementsOf archConns
  }

  it should "parse a complex architecture with IO" in {
    // Grab the complex example architecture file first
    val ioArch = Architecture("src/test/resources/archparse/complex_arch_io.xml")
  }
}
