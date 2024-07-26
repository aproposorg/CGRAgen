package cgragen.hwgen

import cgragen.cgra.{CGRA, confWidth}

import cgragen.hwgen.ConfigurationType._

import chisel3._
import chisel3.util.{Enum, is, switch, isPow2, log2Up}
import chisel3.util.experimental.forceName

import scala.collection.mutable

/** Hardware system wrapper for CGRAs with parameterizable configuration
 * @param mod the CGRA module to wrap
 * @param confType the type of configuration to implement
 * 
 * @note Follows the same construction strategy as in Buildable.
 * 
 * @note The controller has a simple memory-mapped interface style that 
 *       exposes all the configuration registers to the user in alphabetic 
 *       order of their names, determined by the modules and ports they drive.
 * 
 *       This module provides a simple interface to manage up to 
 *       `NumConfiguration` configurations with each `II` contexts, as set out 
 *       in the tool configuration. The interface allows for reading out and 
 *       writing configurations at `InterfaceSize` (>= 32) chunks with simple 
 *       memory-mapped registers.
 * 
 *       The address mapping is, by default, such that the second least 
 *       significant byte distinguishes between general control registers and 
 *       the context registers. Namely:
 *       0     => enable [addr(7, 0)=0], currCnf [addr(7, 0)=1],
 *                iters [addr(7, 0)=2]
 *       1 ... => contxts(0 ...) [addr(8+log2Up(numConfs)-1, 8)-1=0 ...]
 *                (and indirectly active(0 ...))
 *       Asserting intf.en enables reading out values from these registers, 
 *       delayed by a cycle, while values are overriden only when intf.we is 
 *       asserted too.
 */
private[hwgen] final class CGRASystem(mod: CGRA, confType: ConfigurationType)
  (implicit conf: cgragen.Parameters) extends Module {
  require(isPow2(conf.HWInterfaceSize) && conf.HWInterfaceSize >= 32,
    "cannot generate CGRA system for interface sizes of fewer than 32 bits")
  require(isPow2(conf.HWNumConfigurations) && conf.HWNumConfigurations <= 128,
    "cannot generate CGRA system for non-power-of-2 number of configurations")
  require(conf.II <= 8,
    "cannot generate CGRA system for more than eight contexts per configuration")

  private val intfSize = conf.HWInterfaceSize
  private val numConfs = conf.HWNumConfigurations
  private val numCntxs = conf.II

  private val cntxSize  = confWidth(mod)
  private val cntxEntrs = (cntxSize + intfSize - 1) / intfSize
  assert(cntxEntrs < 32,
    "cannot generate CGRA system for contexts wider than 32 times the interface size")

  /** Fields start ***********************************************************/
  // Generate and instantiate the CGRA
  private val cgra = Module(new CGRAModule(mod, confType))

  // Override the desired name of this module
  override val desiredName = s"${cgra.desiredName}_system"

  // Declare the `io` field as a copy of the CGRA's
  val io = IO(new Bundle {
    val ins  = Input (cgra.io.ins .cloneType)
    val outs = Output(cgra.io.outs.cloneType)
  })

  // Connect the top-level IO to the CGRA
  io <> cgra.io
  io.ins .elements.foreach { case (name, port) => forceName(port, name) }
  io.outs.elements.foreach { case (name, port) => forceName(port, name) }

  // Declare the controller interface directly
  val intf = IO(new Bundle {
    val en   = Input(Bool())
    val we   = Input(Bool())
    val addr = Input(UInt(intfSize.W))
    val cin  = Input(UInt(intfSize.W))
    val cout = Output(UInt(intfSize.W))
  })

  // Construct register-based memories for the configurations (not the FSM)
  private val enable  = RegInit(false.B)
  private val currCnf = RegInit(0.U(log2Up(numConfs).W))
  private val iters   = RegInit(0.U(conf.HWLog2NIters.W))
  private val contxs  = RegInit(VecInit(Seq.fill(numConfs) {
    VecInit(Seq.fill(numCntxs) {
      VecInit(Seq.fill(cntxEntrs)(0.U(intfSize.W)))
    })
  }))
  private val active = RegInit(VecInit(Seq.fill(numConfs) {
    VecInit(Seq.fill(numCntxs)(false.B))
  }))
  /** Fields end *************************************************************/

  /**
   * Generate a simple controller interface
   */
  private val busy = WireDefault(true.B)
  private val cout = RegInit(0.U(intfSize.W))
  intf.cout := cout

  when(intf.en) {
    val memDet = intf.addr(intfSize-1, 8)
    val regDet = intf.addr(7, 0)

    when(memDet === 0.U) {
      // Various control registers
      when(regDet === 0.U) {
        // Enable register
        cout := 0.U(31.W) ## enable
        when(intf.we) {
          enable := intf.cin(0)
        }
      }.elsewhen(regDet === 1.U) {
        // Current configuration register
        cout := 0.U((32-currCnf.getWidth).W) ## currCnf
        when(intf.we & !busy) {
          currCnf := intf.cin(currCnf.getWidth-1, 0)
        }
      }.elsewhen(regDet === 2.U) {
        // Number of iterations register
        cout := 0.U((32-iters.getWidth).W) ## iters
        when(intf.we & !busy) {
          iters := intf.cin(iters.getWidth-1, 0)
        }
      }
    }.otherwise {
      // Configuration registers
      val confInd = (memDet - 1.U)(log2Up(numConfs)-1, 0)
      val (cntxInd, entrInd) = (regDet(7, 5), regDet(4, 0))
      cout := contxs(confInd)(cntxInd)(entrInd)
      when(intf.we) {
        contxs(confInd)(cntxInd)(entrInd) := intf.cin

        // Reset the enable register if this is the current configuration
        when(confInd === currCnf) {
          enable := false.B
        }

        // Set the active register of the current context
        active(confInd)(cntxInd) := entrInd === cntxEntrs.U

        // Reset the active registers of the following contexts
        (0 until numCntxs).foreach { c =>
          when(c.U > cntxInd) {
            active(confInd)(c.U) := false.B
          }
        }
      }
    }
  }

  /** 
   * Generate the CGRA-facing part of the controller (a simple FSM)
   */
  private val state_idle :: state_load :: state_execute :: Nil = Enum(3)
  private val state = RegInit(state_idle)
  private val currCntx = RegInit(0.U(log2Up(numCntxs).W))
  cgra.ctrl.ins.elements.keys.foreach { name => cgra.ctrl.ins(name) := 0.U }

  confType match {
    case ParallelConfiguration =>
      // Parallel configuration CGRAs do not need the state_load
      val confDataPorts = cgra.ctrl.ins.elements.filter(!_._1.endsWith("en"))
      val confEnPorts   = cgra.ctrl.ins.elements -- confDataPorts.keys
      assert(confDataPorts.forall(_._1.contains("conf")) && confEnPorts.forall(_._1.contains("conf")))
      assert(confDataPorts.size == confEnPorts.size)

      // Disable the PEs whenever the controller is idle
      cgra.en  := state === state_execute
      cgra.clr := false.B

      // Load a context into the CGRA
      def loadContext(cntx: UInt): Unit = {
        cgra.ctrl.ins.elements.keys
          .filter(_.endsWith("en"))
          .foreach { name => cgra.ctrl.ins(name) := 1.U }
        val confWire = Wire(new PortRecord[UInt](cgra.ctrl.ins.elements
          .filter(!_._1.endsWith("en"))
          .toSeq.sortBy(_._1)
          .map { case (name, port) => name -> port.cloneType }))
        confWire := contxs(currCnf)(cntx).asUInt.asTypeOf(confWire)
        cgra.ctrl.ins.elements.keys
          .filter(!_.endsWith("en"))
          .foreach { name => cgra.ctrl.ins(name) := confWire(name) }
      }

      switch(state) {
        is(state_idle) {
          busy := false.B
          when(enable && iters.orR) {
            loadContext(0.U)
            currCntx := 0.U
            cgra.clr := true.B
            state := state_execute
          }
        }

        is(state_execute) {
          val nextCntx = currCntx + 1.U
          when(currCntx === (numCntxs - 1).U || !active(currCnf)(nextCntx)) {
            // Processing has completed or the next context in the configuration is out of use
            loadContext(0.U)
            currCntx := 0.U
            when(iters.orR) {
              iters := iters - 1.U  
            }.otherwise {
              enable := false.B
              state  := state_idle
            }
          }.otherwise {
            loadContext(nextCntx)
            currCntx := nextCntx
          }
        }
      }

    case SerialConfiguration =>
      // Serial configuration CGRAs do not need 
      assert(cgra.ctrl.ins.elements.size == 2)
      if (conf.HWDebug && numCntxs > 1) {
        print("[DEBUG] Executing multi-context mappings in CGRAs with serial ")
        print("configuration is likely to cause low performance. Consider ")
        println("switching to parallel configuration or changing mapper settings.")
      }

      // Disable the PEs whenever the controller is idle
      cgra.en  := state === state_execute
      cgra.clr := false.B

      val shftCntr = RegInit(0.U(log2Up(cntxSize).W))
      switch(state) {
        is(state_idle) {
          busy := false.B
          when(enable && iters.orR) {
            currCntx := 0.U
            shftCntr := 0.U
            cgra.clr := true.B
            state    := state_load
          }
        }

        is(state_load) {
          cgra.ctrl.ins("scan_en") := 1.U
          cgra.ctrl.ins("scan_in") := contxs(currCnf)(currCntx).asUInt(shftCntr)
          when(shftCntr === (cntxSize - 1).U) {
            shftCntr := 0.U
            state    := state_execute
          }.otherwise {
            shftCntr := shftCntr + 1.U
          }
        }

        is(state_execute) {
          val nextCntx = currCntx + 1.U
          when(currCntx === (numCntxs - 1).U || !active(currCnf)(nextCntx)) {
            // Processing has completed or the next context in the configuration is out of use
            currCntx := 0.U
            when(iters.orR) {
              iters := iters - 1.U
              state := state_load
            }.otherwise {
              enable := false.B
              state  := state_idle
            }
          }.otherwise {
            currCntx := nextCntx
          }
        }
      }

    case _ =>
      // should not occur
  }
}
