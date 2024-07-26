package cgragen.hwgen

import cgragen.archparse.PortType._

import cgragen.cgra.{AbstractModule, AbstractPort}
import cgragen.cgra.primitives._

import cgragen.hwgen.ConfigurationType._
import cgragen.hwgen.primitives._

import chisel3._
import chisel3.util.RegEnable
import chisel3.experimental.noPrefix
import chisel3.experimental.hierarchy.{Definition, Instance, instantiable, public}

import scala.collection.mutable

/** Common parent class for all hardware modules
 * @param mod the module to generate hardware from
 * @param confType the type of configuration to implement
 * 
 * @note Also handles parameterization of HardwareModule IOs to ensure these 
 *       are dynamically generated before any Verilog code is generated.
 *       The user-facing API includes the following methods:
 *       - addInput(name, width)
 *       - addOutput(name, width)
 *       - addCtrlInput(name, width)
 *       - addCtrlOutput(name, width)
 *       Resulting ports are accessible through the `io` and `ctrl` fields.
 * 
 * @note Inheriting classes must define a `build` method that completes their 
 *       hardware construction. This is done to ensure proper expansion of IO 
 *       and underlying functionality. The `build` method is automatically 
 *       called upon construction.
 * 
 * @note Only the top-level should have its configuration logic generated. 
 *       Sub-modules and primitives will have their configuration ports passed 
 *       upward to the top-level.
 * 
 * @note All buildable modules define separate enable `en` and clear `clr` 
 *       ports meant to control the sub-module registers. These signals are 
 *       kept separate from the parameterizable data and control ports to 
 *       ensure proper elaboration.
 */
@instantiable
private[hwgen] abstract class Buildable(mod: AbstractModule, confType: ConfigurationType)
  (implicit conf: cgragen.Parameters) extends Module {
  /** Fields start ***********************************************************/
  // Override the desired name of this module
  override val desiredName = mod.fullPath.replace('.', '_')

  // Inputs and outputs formatted as (name, data type)
  protected val _inputs  = mutable.HashMap.empty[String, UInt]
  protected val _outputs = mutable.HashMap.empty[String, UInt]

  // To follow constraints of the chisel3 Module class, we declare the `io` 
  // field here but mark it as lazy to avoid proactive definition
  @public lazy val io = IO(new Bundle {
    val ins  = Input(new PortRecord[UInt](_inputs.toSeq.sortBy(_._1)))
    val outs = Output(new PortRecord[UInt](_outputs.toSeq.sortBy(_._1)))
  })

  // Control inputs and outputs formatted as (name, data type)
  protected val _ctrlIns  = mutable.HashMap.empty[String, UInt]
  protected val _ctrlOuts = mutable.HashMap.empty[String, UInt]

  // The control ports are instead collected in this ctrl field
  @public lazy val ctrl = IO(new Bundle {
    val ins  = Input(new PortRecord[UInt](_ctrlIns.toSeq.sortBy(_._1)))
    val outs = Output(new PortRecord[UInt](_ctrlOuts.toSeq.sortBy(_._1)))
  })

  // Enable and clear inputs for the data path
  @public val en  = IO(Input(Bool()))
  @public val clr = IO(Input(Bool()))

  // Sub-module instances formatted as (name, module)
  protected val _subModules = mutable.HashMap.empty[String, Buildable]

  // Sub-module definitions and instances formatted as (name, module
  // definition/instance). Only used by the CGRAModule class!
  protected val _defs  = mutable.HashMap.empty[String, Definition[Buildable]]
  protected val _insts = mutable.HashMap.empty[String, Instance[Buildable]]
  /** Fields end *************************************************************/

  // Automatically call the `build` and `buildConfig` methods upon construction
  build()
  buildConfig()
  if (!this.isInstanceOf[CGRAModule]) // @todo Rudimentary check, maybe replace?
    assert(_defs.isEmpty && _insts.isEmpty)

  // Reference the IO fields to ensure their elaboration
  io
  ctrl

  // Connect the enable and clear inputs
  _subModules.keys.foreach { name =>
    _subModules(name).en  := en
    _subModules(name).clr := clr
  }

  /** Modifiers start ********************************************************/
  /** Add a data input port
   * @param name the name of the port to add
   * @param width the width of the port to add
   */
  def addInput(name: String, width: Int): Unit = {
    if (width <= 0) {
      println(s"[ERROR] Cannot add input port ($name) of negative or zero width ($width)")
      throw new Exception(s"invalid input port width $width")
    } else if (_inputs.contains(name)) {
      println(s"[ERROR] Cannot add duplicate input port ($name)")
      throw new Exception(s"duplicate input port $name")
    }
    _inputs(name) = UInt(width.W)
  }

  /** Add a data output port
   * @param name the name of the port to add
   * @param width the width of the port to add
   */
  def addOutput(name: String, width: Int): Unit = {
    if (width <= 0) {
      println(s"[ERROR] Cannot add output port ($name) of negative or zero width ($width)")
      throw new Exception(s"invalid output port width $width")
    } else if (_outputs.contains(name)) {
      println(s"[ERROR] Cannot add duplicate output port ($name)")
      throw new Exception(s"duplicate output port $name")
    }
    _outputs(name) = UInt(width.W)
  }

  /** Add a control input port
   * @param name the name of the port to add
   * @param width the width of the port to add
   */
  def addCtrlInput(name: String, width: Int): Unit = {
    if (width <= 0) {
      println(s"[ERROR] Cannot add control input port ($name) of negative or zero width ($width)")
      throw new Exception(s"invalid control input port width $width")
    } else if (_ctrlIns.contains(name)) {
      println(s"[ERROR] Cannot add duplicate control input port ($name)")
      throw new Exception(s"duplicate control input port $name")
    }
    _ctrlIns(name) = UInt(width.W)
  }

  /** Add a control output port
   * @param name the name of the port to add
   * @param width the width of the port to add
   */
  def addCtrlOutput(name: String, width: Int): Unit = {
    if (width <= 0) {
      println(s"[ERROR] Cannot add control output port ($name) of negative or zero width ($width)")
      throw new Exception(s"invalid control output port width $width")
    } else if (_ctrlOuts.contains(name)) {
      println(s"[ERROR] Cannot add duplicate control output port ($name)")
      throw new Exception(s"duplicate control output port $name")
    }
    _ctrlOuts(name) = UInt(width.W)
  }

  /** Add a sub-module
   * @param name the name of the sub-module to add
   * @param subMod the elaborated module to add
   */
  def addSubModule(name: String, subMod: Buildable): Unit = {
    if (_subModules.contains(name)) {
      println(s"[ERROR] Cannot add duplicate sub-module ($name)")
      throw new Exception(s"duplicate sub-module $name")
    }
    _subModules(name) = subMod
  }
  /** Modifiers end **********************************************************/

  /** Methods start **********************************************************/  
  /** Sign-extend a wire to a given length
   * @param wire the wire to sign-extend
   * @param len the length to sign-extend to
   * @return `wire` sign-extended to length `len`
   */
  def sext(wire: UInt, len: Int): UInt = {
    require(len >= wire.getWidth, "sign-extending to fewer bits is not possible")
    VecInit(Seq.fill(len - wire.getWidth)(wire(wire.getWidth-1))).asUInt ## wire
  }

  /** Zero-extend a wire to a given length
   * @param wire the wire to zero-extend
   * @param len the length to zero-extend to
   * @return `wire` zero-extended to length `len`
   */
  def zext(wire: UInt, len: Int): UInt = {
    require(len >= wire.getWidth, "zero-extending to fewer bits is not possible")
    VecInit(Seq.fill(len - wire.getWidth)(false.B)).asUInt ## wire
  }

  /** Generate the hardware within this module
   * 
   * @note By default, this procedure involves the following steps in the 
   *       specified order:
   *       1. instantiation of sub-modules
   *       2. creation of all ports
   *       3. connection of ports with sub-modules
   *       This order must be maintained in order to ensure the module's IO is 
   *       not referenced before it has been fully specified, avoiding 
   *       unintentional elaboration.
   * 
   * @note Internally, this method transforms any AbstractInputUnits and 
   *       AbstractOutputUnits to simple data ports.
   */
  def build(): Unit = {
    if (conf.HWDebug) println(s"[DEBUG] Building data path in module (${mod.name})")

    // Construct all the sub-modules recursively and add them to the internal 
    // storage for later referencing. Also, keep track of converted 
    // AbstractInputUnits and AbstractOutputUnits
    val convIns  = mutable.HashMap.empty[AbstractPort, String]
    val convOuts = mutable.HashMap.empty[AbstractPort, String]
    mod.subModules.values.foreach { _ match {
      case in: AbstractInputUnit =>
        assert(in.ports.size == 1, "abstract input units should only contain one port")
        val portName = in.name
        convIns(in.ports.values.head) = portName
        if (conf.HWDebug)
          println(s"[DEBUG] Converted input ($portName) in module (${mod.name})")
        addInput(portName, in.dataSize)
      case out: AbstractOutputUnit =>
        assert(out.ports.size == 1, "abstract output units should only contain one port")
        val portName = out.name
        convOuts(out.ports.values.head) = portName
        if (conf.HWDebug)
          println(s"[DEBUG] Converted output ($portName) in module (${mod.name})")
        addOutput(portName, out.dataSize)
      case const: AbstractConstantUnit =>
        if (conf.HWDebug)
          println(s"[DEBUG] Instantiated constant unit (${const.name}) in module (${mod.name})")
        addSubModule(const.name, Module(new ConstantUnit(const)))
      case func: AbstractFunctionUnit =>
        if (conf.HWDebug)
          println(s"[DEBUG] Instantiated function unit (${func.name}) in module (${mod.name})")
        addSubModule(func.name, Module(new FunctionUnit(func)))
      case mux: AbstractMultiplexerUnit =>
        if (conf.HWDebug)
          println(s"[DEBUG] Instantiated multiplexer (${mux.name}) in module (${mod.name})")
        addSubModule(mux.name, Module(new Multiplexer(mux)))
      case reg: AbstractRegisterUnit =>
        if (conf.HWDebug)
          println(s"[DEBUG] Instantiated register (${reg.name}) in module (${mod.name})")
        addSubModule(reg.name, Module(new Register(reg)))
      case rf: AbstractRegisterFileUnit =>
        if (conf.HWDebug)
          println(s"[DEBUG] Instantiated register file (${rf.name}) in module (${mod.name})")
        addSubModule(rf.name, Module(new RegisterFile(rf)))
      case subMod =>
        if (conf.HWDebug)
          println(s"[DEBUG] Instantiated sub-module (${subMod.name}) in module (${mod.name})")
        addSubModule(subMod.name, Module(new SubModule(subMod)))
    }}

    // Extract any unconnected sub-module ports for which to provide 
    // pass-through connection here
    val unConnSubModIns = {
      val allSubModIns = _subModules.map { case (name, subMod) =>
        name -> mutable.HashSet(subMod.io.ins.elements.keys.toSeq:_*) }

      // Remove any connected sink ports
      mod.connections.values
        .flatMap(_.sinks)
        .filter(sink => sink.parent != mod && !convOuts.contains(sink))
        .foreach { sink => allSubModIns(sink.parent.name) -= sink.name }

      allSubModIns
    }

    val unConnSubModOuts = {
      val allSubModOuts = _subModules.map { case (name, subMod) =>
        name -> mutable.HashSet(subMod.io.outs.elements.keys.toSeq:_*) }

      // Remove any connected source ports
      mod.connections.values
        .map(_.source)
        .filter(source => source.parent != mod && !convIns.contains(source))
        .foreach { source => allSubModOuts(source.parent.name) -= source.name }

      allSubModOuts
    }

    // Extract the configuration ports of this module
    val configCellPorts = mod.configCells.values.map(_.port)

    // Add all the required input ports (in addition to the converted ones)
    mod.ports
      .filter  { case (_, port) => port.pt == PortInput && !configCellPorts.exists(_ == port) }
      .foreach { case (name, port) => addInput(name, port.dataSize) }
    unConnSubModIns.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        addInput(name, _subModules(subModName).io.ins(name).getWidth)
      }
    }

    // Add all the required outputs ports (in addition to the converted ones)
    mod.ports
      .filter(_._2.pt == PortOutput)
      .foreach { case (name, port) => addOutput(name, port.dataSize) }
    unConnSubModOuts.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        addOutput(name, _subModules(subModName).io.outs(name).getWidth)
      }
    }

    if (conf.HWDebug) println(s"[DEBUG] Added input and output ports to module (${mod.name})")

    // Connect all ports together as specified in the module
    mod.connections.values.foreach { conn =>
      val source = conn.source

      // Either or both the source and sink can be ports of this module, 
      // instances of converted ports, or ports of sub-modules
      val (sourceName, sourcePort) = if (source.parent == mod) {
        (source.name, io.ins(source.name))
      } else if (convIns.contains(source)) {
        (convIns(source), io.ins(convIns(source)))
      } else {
        (source.name, _subModules(source.parent.name).io.outs(source.name))
      }

      conn.sinks.foreach { sink =>
        val (sinkName, sinkPort) = if (sink.parent == mod) {
          (sink.name, io.outs(sink.name))
        } else if (convOuts.contains(sink)) {
          (convOuts(sink), io.outs(convOuts(sink)))
        } else {
          (sink.name, _subModules(sink.parent.name).io.ins(sink.name))
        }

        // Extend the source port if it is narrower than the sink port
        if (sourcePort.getWidth < sinkPort.getWidth) {
          sinkPort.suggestName(sinkName) := (if (conn.sext) sext(sourcePort, sinkPort.getWidth) else zext(sourcePort, sinkPort.getWidth)).suggestName(sourceName)
        } else {
          sinkPort.suggestName(sinkName) := sourcePort.suggestName(sourceName)
        }
      }
    }

    if (conf.HWDebug) println(s"[DEBUG] Added connections to module (${mod.name})")

    // Pass through any un-connected sub-module inputs
    unConnSubModIns.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        _subModules(subModName).io.ins(name).suggestName(name) := io.ins(name).suggestName(name) }
    }

    // Pass through any un-connected sub-module outputs
    unConnSubModOuts.foreach { case (subModName, portNames) =>
      portNames.foreach { name =>
        io.outs(name).suggestName(name) := _subModules(subModName).io.outs(name).suggestName(name) }
    }

    if (conf.HWDebug) println(s"[DEBUG] Finished building data path in module (${mod.name})")
  }

  /** Generate the configuration hardware within this module */
  def buildConfig(): Unit = {
    if (conf.HWDebug) println(s"[DEBUG] Building configuration logic in module (${mod.name})")

    confType match {
      case ParallelConfiguration =>
        if (conf.HWDebug)
          println(s"[DEBUG] Building parallel-load configuration register in module (${mod.name})")

        // Construct a wide register to capture all the sub-modules' 
        // configuration ports. First create a wire with all the configuration ports
        val confWire = Wire(new PortRecord[PortRecord[UInt]](_subModules
          .toSeq.sortBy(_._1)
          .map { case (name, subMod) => name -> subMod.ctrl.ins.cloneType }))
        val confWidth = confWire.getWidth

        if (confWidth > 0) {
          // Next, add the required ports to this module
          addCtrlInput("conf_en", 1)
          addCtrlInput("conf", confWidth)

          // Finally, create a register and drive the configuration ports with it
          val reg = noPrefix { RegEnable(ctrl.ins("conf"), 0.U(confWidth.W), ctrl.ins("conf_en").orR()) }
          confWire := reg.asTypeOf(confWire)
          _subModules.foreach { case (name, subMod) => subMod.ctrl.ins := confWire(name) }
        }

      case SerialConfiguration =>
        if (conf.HWDebug)
          println(s"[DEBUG] Building serial-load configuration register in module (${mod.name})")

        // Construct a shift register wide enough to capture all the sub-modules' 
        // configuration ports. First create a wire with all the configuration ports
        val confWire = Wire(new PortRecord[PortRecord[UInt]](_subModules
          .toSeq.sortBy(_._1)
          .map { case (name, subMod) => name -> subMod.ctrl.ins.cloneType }))
        val confWidth = confWire.getWidth

        if (confWidth > 0) {
          // Next, add the required ports to this module
          addCtrlInput("scan_en", 1)
          addCtrlInput("scan_in", 1)
          addCtrlOutput("scan_out", 1)

          // Finally, create a shift register and drive the configuration ports with it
          val reg = RegInit(0.U(confWidth.W))
          noPrefix { when(ctrl.ins("scan_en").orR()) {
            reg := reg ## ctrl.ins("scan_in")
          }}
          noPrefix { ctrl.outs("scan_out") := reg(confWidth-1) }
          confWire := reg.asTypeOf(confWire)
          _subModules.foreach { case (name, subMod) => subMod.ctrl.ins := confWire(name) }
        }

      case NoConfiguration =>
        if (conf.HWDebug)
          println(s"[DEBUG] Passing through configuration ports in module (${mod.name})")

        // Pass sub-module configuration ports to this module's IO
        val bindings = _subModules.flatMap { case (name, subMod) =>
          // Sub-modules should not contain any configuration outputs
          if (subMod.ctrl.outs.elements.nonEmpty) {
            println(s"[ERROR] Sub-module ($name) cannot have any configuration outputs")
            throw new Exception("sub-module has configuration output")
          }

          // Create matching ports for each configuration port in the sub-module 
          // in this module's IO and return their destination port
          subMod.ctrl.ins.elements
            .toSeq.sortBy(_._1)
            .map { case (portName, port) =>
            val resName = s"${name}_${portName}"
            addCtrlInput(resName, port.getWidth)
            (resName, port)
          }
        }

        // Now properly bind all the ports
        noPrefix { bindings.foreach { case (portName, destination) =>
          destination := ctrl.ins(portName)
        }}
    }

    if (conf.HWDebug)
      println(s"[DEBUG] Finished building configuration logic in module (${mod.name})")
  }
  /** Methods end ************************************************************/
}

/** Hardware top-module with parameterizable configuration logic
 * @param mod the module to generate hardware from
 * @param confType the type of configuration to implement
 */
private[hwgen] class TopModule(mod: AbstractModule, confType: ConfigurationType)
  (implicit conf: cgragen.Parameters) extends Buildable(mod, confType)

/** Hardware sub-module without configuration logic
 * @param mod the module to generate hardware from
 */
private[hwgen] class SubModule(mod: AbstractModule)
  (implicit conf: cgragen.Parameters) extends Buildable(mod, NoConfiguration) {
  require(ctrl.outs.elements.isEmpty,
    "sub-module cannot contain any control outputs")
}
