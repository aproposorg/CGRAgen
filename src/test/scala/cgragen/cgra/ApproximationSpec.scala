package cgragen.cgra

import cgragen.archparse.Architecture

import cgragen.TestConfiguration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ApproximationSpec extends AnyFlatSpec with TestConfiguration {
  behavior of "Architecture-to-CGRA converter"

  it should "leave non-approximable function unit unchanged" in {
    val locParams = params.copy(approximateArithmetic = true)
    val cgra = CGRA(Architecture(nonApprxmblArch))(locParams)

    // Validate some overall characteristics first
    cgra.dataSize should be (params.DataSize)
    cgra.subModules.keys should (have size 1 and contain allElementsOf Seq("block_0_0"))

    // Then check that the instantiated module has the right parameters
    val block = cgra.subModules("block_0_0")
    block.ports.keys should (
      have size 4 and
      contain allElementsOf Seq("in0", "in1", "out0", "out1"))
    block.subModules.keys should contain allElementsOf Seq("fu", "rgstr0", "rgstr1")

    // Finally check the instantiated function unit
    val fu = block.subModules("fu")
    fu.ports.keys should (
      have size 4 and
      contain allElementsOf Seq("in_a", "in_b", "out", "select"))
    fu.configCells should have size 1
  }

  it should "capture locally specified approximable function unit" in {
    val cgra = CGRA(Architecture(apprxmblArch))

    // Validate some overall characteristics first
    val pe0s = (0 until 2).map(c => s"block_0_$c")
    val pe1s = (0 until 2).map(c => s"block_1_$c")
    cgra.dataSize should be (params.DataSize)
    cgra.subModules.keys should (
      have size 4 and
      contain allElementsOf (pe0s ++ pe1s))

    // Then check that the instantiated modules have the right parameters, and 
    // that their function units have the proper approximations
    pe0s.foreach { name =>
      val block = cgra.subModules(name)
      block.ports.keys should (
        have size 4 and
        contain allElementsOf Seq("in0", "in1", "out0", "out1"))
      block.subModules.keys should contain allElementsOf Seq("fu", "rgstr0", "rgstr1")

      val fu = block.subModules("fu")
      fu.ports.keys should (
        have size 4 and
        contain allElementsOf Seq("in_a", "in_b", "out", "select"))
      fu.configCells should have size 1
    }

    pe1s.foreach { name =>
      val block = cgra.subModules(name)
      block.ports.keys should (
        have size 4 and
        contain allElementsOf Seq("in0", "in1", "out0", "out1"))
      block.subModules.keys should contain allElementsOf Seq("fu", "rgstr")

      val fu = block.subModules("fu")
      fu.ports.keys should (
        have size 5 and
        contain allElementsOf Seq("in_a", "in_b", "out", "select", "mode"))
      fu.configCells should have size 2
    }
  }

  it should "capture globally specified approximable function unit" in {
    val locParams = params.copy(approximateArithmetic = true)
    val cgra = CGRA(Architecture(apprxmblArch))(locParams)
    
    // Validate some overall characteristics first
    val pe0s = (0 until 2).map(c => s"block_0_$c")
    val pe1s = (0 until 2).map(c => s"block_1_$c")
    cgra.dataSize should be (params.DataSize)
    cgra.subModules.keys should (
      have size 4 and
      contain allElementsOf (pe0s ++ pe1s))

    // Then check that the instantiated modules have the right parameters, and 
    // that their function units have the proper approximations
    pe0s.foreach { name =>
      val block = cgra.subModules(name)
      block.ports.keys should (
        have size 4 and
        contain allElementsOf Seq("in0", "in1", "out0", "out1"))
      block.subModules.keys should contain allElementsOf Seq("fu", "rgstr0", "rgstr1")
    }
    pe1s.foreach { name =>
      val block = cgra.subModules(name)
      block.ports.keys should (
        have size 4 and
        contain allElementsOf Seq("in0", "in1", "out0", "out1"))
      block.subModules.keys should contain allElementsOf Seq("fu", "rgstr")
    }
    (pe0s ++ pe1s).foreach { name =>
      val fu = cgra.subModules(name).subModules("fu")
      fu.ports.keys should (
        have size 5 and
        contain allElementsOf Seq("in_a", "in_b", "out", "select", "mode"))
      fu.ports("mode").dataSize should be (log2Ceil(locParams.CGRAApproximationModes + 1))
      fu.configCells should have size 2
    }
  }
}
