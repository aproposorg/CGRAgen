package cgragen.cgra

import cgragen.archparse.Architecture

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CGRASpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Architecture-to-CGRA converter"

  it should "convert a simple architecture" in {
    // First, grab the example architecture and parse it
    val simpleArch = Architecture("src/test/resources/archparse/simple_arch.xml")

    // Next, convert it to a CGRA
    val simpleCgra = CGRA(simpleArch)

    // Then validate that it contains the expected output
    simpleCgra.dataSize should be (params.DataSize)
    simpleCgra.ports should be (empty)
    
    val cgraSubMods = (0 until 2).flatMap { row => 
      (0 until 2).map { col => 
        s"block_${row}_${col}"
      }
    }
    simpleCgra.subModules should have size cgraSubMods.length
    simpleCgra.subModules.map(_._1) should contain allElementsOf cgraSubMods

    // ... run through each of the sub-modules and check its contents
    cgraSubMods.foreach { subModName =>
      simpleCgra.subModules.keys should contain (subModName)
      val subMod = simpleCgra.subModules(subModName)

      // Check its ports
      val subModInputs  = (0 until 4).map(i => s"in$i")
      val subModOutputs = IndexedSeq("out")
      val subModPorts   = (subModInputs ++ subModOutputs)
      subMod.ports should have size subModPorts.size
      subMod.ports.keys should contain allElementsOf subModPorts
      
      // Check its sub-modules
      val blockSubMods = IndexedSeq("rgstr", "func", "mux_func_in_a", "mux_func_in_b")
      blockSubMods.foreach { primName =>
        subMod.subModules.keys should contain (primName)
        val primitive = subMod.subModules(primName)
        primName match {
          case "rgstr"  =>
            primitive.ports should have size 2
            primitive.ports.keys should (contain ("in") and contain ("out"))
          case "func" =>
            primitive.ports should have size 4
            primitive.ports.keys should (contain ("in_a") and contain ("in_b"))
            primitive.ports.keys should (contain ("select") and contain ("out"))
          case _ =>
            primitive.ports should have size 6
            primitive.ports.keys should contain allElementsOf ((0 until 4).map(i => s"in$i"))
            primitive.ports.keys should (contain ("select") and contain ("out"))
        }
      }

      // Check its top-level connections
      subModInputs.foreach { input =>
        val conn = subMod.connections(subMod.ports(input))
        val dsts = (0 until 2).map(_ => s"$input")
        conn.sinks.map(_.name) should contain allElementsOf dsts
      }
      val outConn = subMod.connections(subMod.subModules("rgstr").ports("out"))
      outConn.sinks should contain (subMod.ports("out"))
    }

    // ... run through the top-level connections
    (0 until 2).foreach { i =>
      // Check vertical connections
      val vertDWSrcMod = simpleCgra.subModules(s"block_0_$i")
      val vertDWDstMod = simpleCgra.subModules(s"block_1_$i")
      simpleCgra.connections(vertDWSrcMod.ports("out")).sinks should contain (vertDWDstMod.ports("in0"))
      val vertUWSrcMod = vertDWDstMod
      val vertUWDstMod = vertDWSrcMod
      simpleCgra.connections(vertUWSrcMod.ports("out")).sinks should contain (vertUWDstMod.ports("in2"))

      // Check horizontal connections
      val horzEWSrcMod = simpleCgra.subModules(s"block_${i}_0")
      val horzEWDstMod = simpleCgra.subModules(s"block_${i}_1")
      simpleCgra.connections(horzEWSrcMod.ports("out")).sinks should contain (horzEWDstMod.ports("in3"))
      val horzWWSrcMod = horzEWDstMod
      val horzWWDstMod = horzEWSrcMod
      simpleCgra.connections(horzWWSrcMod.ports("out")).sinks should contain (horzWWDstMod.ports("in1"))
    }
  }

  it should "convert a complex architecture" in {
    // First, grab the example architecture and parse it
    val complexArch = Architecture("src/test/resources/archparse/complex_arch.xml")

    // Next, convert it to a CGRA
    val complexCgra = CGRA(complexArch)

    // then validate that it contains the expected output
    complexCgra.dataSize should be (params.DataSize)
    complexCgra.ports should be (empty)

    val cgraSubMods = (0 until 4).flatMap { row =>
      (1 until 5).map(col => s"block_${row}_${col}")
    } :+ "block_0_0" // manually add the register file
    complexCgra.subModules.keys should (have size cgraSubMods.size and contain allElementsOf cgraSubMods)

    // ... run through each sub-module and check their content
    // ... starting with the register file (block_0_0)
    val rfSubMod = complexCgra.subModules("block_0_0")

    // Check its ports
    val rfInputs  = (0 until 4).map(i => s"in$i")
    val rfOutputs = (0 until 4).map(i => s"out$i")
    val rfPorts   = (rfInputs ++ rfOutputs)
    rfSubMod.ports should have size rfPorts.length
    rfSubMod.ports.keys should contain allElementsOf rfPorts

    // Check its sub-modules
    rfSubMod.subModules.keys should contain ("rf")
    val rfPrimitive = rfSubMod.subModules("rf")
    rfPrimitive.ports should have size 20 // 4 x input data and address pairs, output data and address pairs, and WEs
    (0 until 4).foreach { i =>
      rfPrimitive.ports.keys should (contain (s"in$i") and contain (s"addr_in$i") and contain (s"WE$i"))
      rfPrimitive.ports.keys should (contain (s"out$i") and contain (s"addr_out$i"))
    }

    // Check its top-level connections
    rfInputs.foreach { input =>
      val conn = rfSubMod.connections(rfSubMod.ports(input))
      conn.sinks should (have length 1 and contain (rfSubMod.subModules("rf").ports(input)))
    }
    rfOutputs.foreach { output =>
      val conn = rfSubMod.connections(rfSubMod.subModules("rf").ports(output))
      conn.sinks should (have length 1 and contain (rfSubMod.ports(output)))
    }

    // ... and continuing on to the PEs
    cgraSubMods.filter(_ != "block_0_0").foreach { subModName =>
      val subMod = complexCgra.subModules(subModName)

      // Check its ports
      val subModInputs  = (0 until 4).map(i => s"in$i")
      val subModOutputs = (0 until 4).map(i => s"out$i")
      val subModPorts   = subModInputs ++ subModOutputs
      subMod.ports should have size subModPorts.length
      subMod.ports.keys should contain allElementsOf subModPorts

      // Check its sub-modules
      val blockSubMods = {
        val bpMuxes  = (0 until 4).map(i => s"mux_bp$i")
        val opMuxes  = (0 until 2).map(i => s"mux_reg_op${i}_in")
        val outMuxes = (0 until 4).map(i => s"mux_out$i")
        val inRegs   = (0 until 4).map(i => s"reg$i")
        val opRegs   = (0 until 2).map(i => s"reg_op$i")
        bpMuxes ++ opMuxes ++ outMuxes ++ inRegs ++ opRegs ++ Seq("reg_res", "const", "alu")
      }
      blockSubMods.foreach { primName =>
        subMod.subModules.keys should contain (primName)
        val primitive = subMod.subModules(primName)
        primName match {
          case "const" =>
            primitive.dataSize should be (16)
            primitive.ports should have size 2
            primitive.ports.keys should (contain ("const") and contain ("out"))
          case "alu" =>
            primitive.ports should have size 4
            primitive.ports.keys should (contain ("in_a") and contain ("in_b"))
            primitive.ports.keys should (contain ("select") and contain ("out"))
          case x if x.startsWith("reg") =>
            primitive.ports should have size 2
            primitive.ports.keys should (contain ("in") and contain ("out"))
          case x if x.startsWith("mux_bp") => // bypass connections
            primitive.ports should have size 4
            primitive.ports.keys should contain allElementsOf ((0 until 2).map(i => s"in$i"))
            primitive.ports.keys should (contain ("select") and contain ("out"))
          case x if x.startsWith("mux_reg_op") =>
            primitive.ports should have size 8
            primitive.ports.keys should contain allElementsOf ((0 until 6).map(i => s"in$i"))
            primitive.ports.keys should (contain ("select") and contain ("out"))
          case _ => // all the others multiplexers
            primitive.ports should have size 9
            primitive.ports.keys should contain allElementsOf ((0 until 7).map(i => s"in$i"))
            primitive.ports.keys should (contain ("select") and contain ("out"))
        }
      }

      // Check its top-level connections
      subModInputs.foreach { case s"in$i" =>
        val conn  = subMod.connections(subMod.ports(s"in$i"))
        val regIn = subMod.subModules(s"reg$i").ports("in")
        val muxIn = subMod.subModules(s"mux_bp$i").ports(s"in0")
        conn.sinks should (have length 2 and contain (regIn) and contain (muxIn))
      }
      subModOutputs.foreach { case s"out$i" =>
        val conn = subMod.connections(subMod.subModules(s"mux_out$i").ports("out"))
        conn.sinks should contain (subMod.ports(s"out$i"))
      }
    }

    // ... run through the top-level connections
    (0 until 4).foreach { row =>
      // Check register file connections
      val rfPort = complexCgra.connections(complexCgra.subModules("block_0_0").ports(s"out$row"))
      val pePort = complexCgra.connections(complexCgra.subModules(s"block_${row}_1").ports("out3"))
      rfPort.sinks should contain (complexCgra.subModules(s"block_${row}_1").ports("in3"))
      pePort.sinks should contain (complexCgra.subModules("block_0_0").ports(s"in$row"))

      // Check vertical connections
      (1 until 5).foreach { col =>
        val vertDWSrcMod = complexCgra.subModules(s"block_${row}_${col}")
        val vertDWDstMod = complexCgra.subModules(s"block_${(row+1)%4}_${col}")
        complexCgra.connections(vertDWSrcMod.ports("out2")).sinks should contain (vertDWDstMod.ports("in0"))
        val vertUWSrcMod = vertDWDstMod
        val vertUWDstMod = vertDWSrcMod
        complexCgra.connections(vertUWSrcMod.ports("out0")).sinks should contain (vertUWDstMod.ports("in2"))
      }

      // Check horizontal connections
      (1 until 4).foreach { col =>
        val horzEWSrcMod = complexCgra.subModules(s"block_${row}_${col}")
        val horzEWDstMod = complexCgra.subModules(s"block_${row}_${col+1}")
        complexCgra.connections(horzEWSrcMod.ports("out1")).sinks should contain (horzEWDstMod.ports("in3"))
        val horzWWSrcMod = horzEWDstMod
        val horzWWDstMod = horzEWSrcMod
        complexCgra.connections(horzWWSrcMod.ports("out3")).sinks should contain (horzWWDstMod.ports("in1"))
      }
    }
  }

  it should "convert a complex hierarchical architecture" in {
    // First, grab the example architecture and parse it
    val hierArch = Architecture("src/test/resources/archparse/complex_hier_arch.xml")

    // Next, convert it to a CGRA
    val hierCgra = CGRA(hierArch)

    // then valudate that it contains the expected output
    hierCgra.dataSize should be (params.DataSize)
    hierCgra.ports should be (empty)

    val cgraSubMods = (0 until 4).flatMap { row =>
      (1 until 5).map(col => s"block_${row}_${col}")
    } :+ "block_0_0" // manually add the register file
    hierCgra.subModules.keys should (have size cgraSubMods.length and contain allElementsOf cgraSubMods)

    // ... run through each sub-module and check their content
    // ... starting with the register file (block_0_0)
    val rfSubMod = hierCgra.subModules("block_0_0")

    // Check its ports
    val rfInputs  = (0 until 4).map(i => s"in$i")
    val rfOutputs = (0 until 4).map(i => s"out$i")
    val rfPorts   = (rfInputs ++ rfOutputs)
    rfSubMod.ports should have size rfPorts.length
    rfSubMod.ports.keys should contain allElementsOf rfPorts

    // Check its sub-modules
    rfSubMod.subModules.keys should contain ("rf")
    val rfPrimitive = rfSubMod.subModules("rf")
    rfPrimitive.ports should have size 20 // 4 x input data and address pairs, output data and address pairs, and WEs
    (0 until 4).foreach { i =>
      rfPrimitive.ports.keys should (contain (s"in$i") and contain (s"addr_in$i") and contain (s"WE$i"))
      rfPrimitive.ports.keys should (contain (s"out$i") and contain (s"addr_out$i"))
    }

    // Check its top-level connections
    rfInputs.foreach { input =>
      val conn = rfSubMod.connections(rfSubMod.ports(input))
      conn.sinks should (have length 1 and contain (rfSubMod.subModules("rf").ports(input)))
    }
    rfOutputs.foreach { output =>
      val conn = rfSubMod.connections(rfSubMod.subModules("rf").ports(output))
      conn.sinks should (have length 1 and contain (rfSubMod.ports(output)))
    }

    // ... and continuing on to the PEs
    cgraSubMods.filter(_ != "block_0_0").foreach { subModName =>
      val subMod = hierCgra.subModules(subModName)

      // Check its ports
      val subModInputs  = (0 until 4).map(i => s"in$i")
      val subModOutputs = (0 until 4).map(i => s"out$i")
      val subModPorts   = subModInputs ++ subModOutputs
      subMod.ports.keys should (have size subModPorts.length and contain allElementsOf subModPorts)

      // Check its sub-modules
      val blockSubMods = {
        val aluMuxes = (0 until 2).map(i => s"mux_alu_op$i")
        aluMuxes ++ Seq("const", "bp", "alu", "xb")
      }
      blockSubMods.foreach { name =>
        subMod.subModules.keys should contain (name)
        val module = subMod.subModules(name)
        name match {
          case "const" =>
            module.dataSize should be (16)
            module.ports.keys should (have size 2 and contain ("const") and contain ("out"))
          case "bp" =>
            val ports = {
              val ins  = (0 until 4).map(i => s"in$i")
              val outs = (0 until 4).map(i => s"out$i")
              ins ++ outs
            }
            module.ports.keys should (have size ports.length and contain allElementsOf ports)
            val prims = {
              val regs  = (0 until 4).map(i => s"reg$i")
              val muxes = (0 until 4).map(i => s"mux_out$i")
              regs ++ muxes
            }
            module.subModules.keys should (have size prims.length and contain allElementsOf prims)
          case "alu" =>
            val ports = Seq("op0", "op1", "alu_out", "reg_out")
            module.ports.keys should (have size ports.length and contain allElementsOf ports)
            val prims = Seq("reg_op0", "reg_op1", "reg_res", "alu")
            module.subModules.keys should (have size prims.length and contain allElementsOf prims)
          case "xb" =>
            val ports = {
              val ins  = (0 until 7).map(i => s"in$i")
              val outs = (0 until 4).map(i => s"out$i")
              ins ++ outs
            }
            module.ports.keys should (have size ports.length and contain allElementsOf ports)
            val prims = (0 until 4).map(i => s"mux_out$i")
            module.subModules.keys should (have size prims.length and contain allElementsOf prims)
          case _ => // the two multiplexers
            val ports = (0 until 6).map(i => s"in$i") ++ Seq("select", "out")
            module.ports.keys should (have size ports.length and contain allElementsOf ports)
        }
      }

      // Check its top-level connections
      subModInputs.foreach { portName =>
        val conn = subMod.connections(subMod.ports(portName))
        conn.sinks should (have length 1 and contain (subMod.subModules("bp").ports(portName)))
      }
      subModOutputs.foreach { portName =>
        val conn = subMod.connections(subMod.subModules("xb").ports(portName))
        conn.sinks should contain (subMod.ports(portName))
      }
    }

    // ... run through the top-level connections
    (0 until 4).foreach { row =>
      // Check register file connections
      val rfPort = hierCgra.connections(hierCgra.subModules("block_0_0").ports(s"out$row"))
      val pePort = hierCgra.connections(hierCgra.subModules(s"block_${row}_1").ports("out3"))
      rfPort.sinks should contain (hierCgra.subModules(s"block_${row}_1").ports("in3"))
      pePort.sinks should contain (hierCgra.subModules("block_0_0").ports(s"in$row"))

      // Check vertical connections
      (1 until 5).foreach { col =>
        val vertDWSrcMod = hierCgra.subModules(s"block_${row}_${col}")
        val vertDWDstMod = hierCgra.subModules(s"block_${(row+1)%4}_${col}")
        hierCgra.connections(vertDWSrcMod.ports("out2")).sinks should contain (vertDWDstMod.ports("in0"))
        val vertUWSrcMod = vertDWDstMod
        val vertUWDstMod = vertDWSrcMod
        hierCgra.connections(vertUWSrcMod.ports("out0")).sinks should contain (vertUWDstMod.ports("in2"))
      }

      // Check horizontal connections
      (1 until 4).foreach { col =>
        val horzEWSrcMod = hierCgra.subModules(s"block_${row}_${col}")
        val horzEWDstMod = hierCgra.subModules(s"block_${row}_${col+1}")
        hierCgra.connections(horzEWSrcMod.ports("out1")).sinks should contain (horzEWDstMod.ports("in3"))
        val horzWWSrcMod = horzEWDstMod
        val horzWWDstMod = horzEWSrcMod
        hierCgra.connections(horzWWSrcMod.ports("out3")).sinks should contain (horzWWDstMod.ports("in1"))
      }
    }
  }
}
