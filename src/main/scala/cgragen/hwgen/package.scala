package cgragen

import cgragen.cgra.{AbstractModule, CGRA}
import cgragen.cgra.ModuleType._

import chisel3._
import chisel3.internal.requireIsChiselType

import scala.collection.immutable

package object hwgen {
  // Configuration type enumeration
  private[hwgen] object ConfigurationType extends Enumeration {
    type ConfigurationType    = Value
    val ParallelConfiguration = Value("parallel")
    val SerialConfiguration   = Value("serial")
    val NoConfiguration       = Value("none")
  }
  import ConfigurationType._

  /** Record of port names used for managing paramterizable IO in modules
   * @param elts the sequence of port names to ports
   */
  private[hwgen] class PortRecord[T <: Data](elts: Seq[(String, T)]) extends Record {
    elts.map(_._2).foreach(requireIsChiselType(_))
    val elements = immutable.ListMap(elts:_*)

    def apply(elt: String): T = elements(elt)

    override def cloneType: this.type = {
      (new PortRecord(elts.map { case (elt, field) => elt -> field.cloneType }))
        .asInstanceOf[this.type]
    }
  }

  private[cgragen] object HWGen {
    /** Generate the Verilog description of an AbstractModule and return it in a string
     * @param mod the module to generate hardware from
     * @return the Verilog description of `mod`
     * 
     * @note Should only be called on the top-level module for which to generate 
     *       hardware. Sub-modules are automatically generated within the top-level.
     */
    def apply(mod: AbstractModule)(implicit conf: Parameters): String = {
      val confType = if (conf.HWParallelConfiguration) ParallelConfiguration else SerialConfiguration
      apply(mod, confType)
    }

    /** Generate the Verilog description of an AbstractModule and return it in a string
     * @param mod the module to generate hardware from
     * @param confType the type of configuration hardware to implement
     * @return the Verilog description of `mod`
     */
    def apply(mod: AbstractModule, confType: ConfigurationType)(implicit conf: Parameters): String = {
      require(mod.modType == ModComposite, "non-composite modules cannot be at the top-level")
      getVerilogString(new TopModule(mod, confType), conf.HWCompilerOptions)
    }

    /** Generate the Verilog description of a CGRA and return it in a string
     * @param cgra the CGRA to generate hardware from
     * @return the Verilog description of `cgra`
     * 
     * @note Should only be called on the top-level CGRA module.
     */
    def apply(mod: CGRA)(implicit conf: Parameters): String = {
      val confType = if (conf.HWParallelConfiguration) ParallelConfiguration else SerialConfiguration
      getVerilogString(new CGRAModule(mod, confType), conf.HWCompilerOptions)
    }
  }

  private[cgragen] object SysGen {
    /** Generate the Verilog description of a CGRA system and return it in a string
     * @param cgra the CGRA to generate hardware from
     * @return the Verilog description of `cgra` wrapped in a simple controller
     * 
     * @note Should only be called on the top-level CGRA module.
     */
    def apply(mod: CGRA)(implicit conf: cgragen.Parameters): String = {
      val confType = if (conf.HWParallelConfiguration) ParallelConfiguration else SerialConfiguration
      getVerilogString(new CGRASystem(mod, confType), conf.HWCompilerOptions)
    }
  }
}
