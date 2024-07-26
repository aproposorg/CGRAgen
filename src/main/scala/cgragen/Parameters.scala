package cgragen

/** Parameters needed to configure CGRAgen */
private[cgragen] final class Parameters(
  // Debug prints
  val CGRADebug  : Boolean, 
  val MRRGDebug  : Boolean, 
  val DFGDebug   : Boolean, 
  val MapperDebug: Boolean, 
  val HWDebug    : Boolean, 

  // HWgen options
  val HWGenerate: Boolean, 
  val HWParallelConfiguration: Boolean, 
  val HWCompilerOptions: Array[String], 
  val HWInterfaceSize: Int, 
  val HWNumConfigurations: Int, 
  val HWLog2NIters: Int, 

  // DFG options
  val DFGPropagateConstants: Boolean, 
  val DFGAvoidMulticastConstants: Boolean, 
  val DFGRemoveDeadNodes: Boolean, 
  val DFGLimitFanout: Boolean, 
  val DFGMaxFanout: Int, 

  // CGRA options
  val CGRAInferTopLevelIO: Boolean, 
  val CGRATopLevelIOName: String, 
  val CGRAApproximationModes: Int, 
  val CGRAApproximableOps: String, 
  val CGRAApproximationWidthFraction: Double, 

  val ApproximateArithmetic: Boolean, 

  val DataSize: Int, 
  val Sext: Boolean, 
  val II: Int, 
  val FUArgs: Map[String, String], 
  val RFArgs: Map[String, String], 

  // Mapper options
  val MapperType: cgragen.mapping.MapperType.MapperType, 
  val ApproximatorType: cgragen.mapping.ApproximatorType.ApproximatorType, 
  val MapperTimeLimit: Double, 
  val MapperArgs: Map[String, String]
) {
  /** Return a copy of this set of parameters */
  def copy(
    cgraDebug  : Boolean = CGRADebug, 
    mrrgDebug  : Boolean = MRRGDebug, 
    dfgDebug   : Boolean = DFGDebug, 
    mapperDebug: Boolean = MapperDebug, 
    hwDebug    : Boolean = HWDebug, 

    hwGenerate: Boolean = HWGenerate, 
    hwParallelConfiguration: Boolean = HWParallelConfiguration, 
    hwCompilerOptions: Array[String] = HWCompilerOptions, 
    hwInterfaceSize: Int = HWInterfaceSize, 
    hwNumConfigurations: Int = HWNumConfigurations, 
    hwLog2NIters: Int = HWLog2NIters, 

    dfgPropagateConstants: Boolean = DFGPropagateConstants, 
    dfgAvoidMulticastConstants: Boolean = DFGAvoidMulticastConstants, 
    dfgRemoveDeadNodes: Boolean = DFGRemoveDeadNodes, 
    dfgLimitFanout: Boolean = DFGLimitFanout, 
    dfgMaxFanout: Int = DFGMaxFanout, 

    cgraInferTopLevelIO: Boolean = CGRAInferTopLevelIO, 
    cgraTopLevelIOName: String = CGRATopLevelIOName, 
    cgraApproximationModes: Int = CGRAApproximationModes, 
    cgraApproximableOps: String = CGRAApproximableOps, 
    cgraApproximationWidthFraction: Double = CGRAApproximationWidthFraction, 

    approximateArithmetic: Boolean = ApproximateArithmetic,

    dataSize: Int = DataSize, 
    sext: Boolean = Sext, 
    ii: Int = II, 
    fuArgs: Map[String, String] = FUArgs, 
    rfArgs: Map[String, String] = RFArgs, 

    mapperType: cgragen.mapping.MapperType.MapperType = MapperType, 
    approximatorType: cgragen.mapping.ApproximatorType.ApproximatorType = ApproximatorType, 
    mapperTimeLimit: Double = MapperTimeLimit, 
    mapperArgs: Map[String, String] = MapperArgs
  ): Parameters = new Parameters(
    cgraDebug, mrrgDebug, dfgDebug, mapperDebug, hwDebug,
    hwGenerate, hwParallelConfiguration, hwCompilerOptions, hwInterfaceSize, hwNumConfigurations, hwLog2NIters,
    dfgPropagateConstants, dfgAvoidMulticastConstants, dfgRemoveDeadNodes, dfgLimitFanout, dfgMaxFanout,
    cgraInferTopLevelIO, cgraTopLevelIOName, cgraApproximationModes, cgraApproximableOps, cgraApproximationWidthFraction,
    approximateArithmetic,
    dataSize, sext, ii, fuArgs, rfArgs,
    mapperType, approximatorType, mapperTimeLimit, mapperArgs)

  /** Return a string representation of this set of parameters */
  override def toString(): String = {
    val fieldsAsPairs = for (field <- this.getClass().getDeclaredFields()) yield {
      field.setAccessible(true)
      (field.getName, field.get(this))
    }
    val fields = Map.from(fieldsAsPairs)
    s"""Parameters(
    |${fields.map { case (fieldName, fieldObj) => s"  $fieldName -> $fieldObj" }.mkString("\n")}
    |)""".stripMargin
  }
}

private[cgragen] object Parameters {
  /** Generate a new parameter configuration of CGRAgen given a map of 
   * (String -> String) pairs
   * @param params a map of configuration parameters
   * @return a new instance of [[Parameters]]
   * 
   * @note Throws an exception if a mandatory configuration parameter is missing.
   */
  def apply(params: Map[String, String]) = try {
    // First, verify that the mandatory defaults to function units and 
    // register files are present
    val fuArgs = params.collect { case (k, v) if k.startsWith("cgra.FuncUnit") =>
      (k.split('.').last -> v)
    }
    val rfArgs = params.collect { case (k, v) if k.startsWith("cgra.RegisterFile") =>
      (k.split('.').last -> v)
    }
    if (!List("II", "Latency", "Ops").forall(fuArgs.contains(_))) {
      println("""Error! CGRAgen is missing a function unit default parameter. 
                |At least three parameters are needed:
                |- cgra.FuncUnit.II
                |- cgra.FuncUnit.Latency
                |- cgra.FuncUnit.Ops""".stripMargin)
      throw new Exception("missing function unit default parameter")
    }
    if (!List("Log2NRegister").forall(rfArgs.contains(_))) {
      println("""Error! CGRAgen is missing a register file default parameter.
                |At least one parameter is needed:
                |- cgra.RegisterFile.Log2NRegister""".stripMargin)
      throw new Exception("missing register file default parameter")
    }

    // Then, return a new Parameters object
    val mapper = params("mapper.Mapper")
    val approximator = params("mapper.Approximator")
    new Parameters(
      // Debug prints
      params("cgra.DebugPrints")  .toInt != 0, 
      params("mrrg.DebugPrints")  .toInt != 0, 
      params("dfg.DebugPrints")   .toInt != 0, 
      params("mapper.DebugPrints").toInt != 0, 
      params("hw.DebugPrints")    .toInt != 0,

      // HWgen operations
      params("hw.Generate")             .toInt != 0, 
      params("hw.ParallelConfiguration").toInt != 0, 
      params("hw.CompilerOptions")      .split(' '), 
      params("hw.InterfaceSize")        .toInt, 
      params("hw.NumConfigurations")    .toInt, 
      params("hw.Log2NIters")           .toInt, 

      // DFG options
      params("dfg.PropagateConstants")     .toInt != 0, 
      params("dfg.AvoidMulticastConstants").toInt != 0, 
      params("dfg.RemoveDeadNodes")        .toInt != 0, 
      params("dfg.LimitFanout")            .toInt != 0, 
      params("dfg.MaxFanout")              .toInt, 

      // CGRA options
      params("cgra.InferTopLevelIO")           .toInt != 0, 
      params("cgra.TopLevelIOName"), 
      params("cgra.ApproximationModes")        .toInt, 
      params("cgra.ApproximableOps"), 
      params("cgra.ApproximationWidthFraction").toDouble, 

      params("cgra.ApproximateArithmetic")     .toInt != 0, 

      params("cgra.DataSize")                  .toInt, 
      params("cgra.Sext")                      .toInt != 0, 
      params("cgra.II")                        .toInt, 
      fuArgs, 
      rfArgs, 

      // Mapper options
      cgragen.mapping.MapperType.withName(mapper.toLowerCase()), 
      cgragen.mapping.ApproximatorType.withName(approximator.toLowerCase()), 
      params("mapper.TimeLimit").toDouble, 
      params.collect { case (k, v) if k.startsWith(s"mapper.$mapper") || k.startsWith("mapper.any") => (k.split('.').last -> v) }
    )
  } catch {
    case x: NoSuchElementException =>
      print("[ERROR] CGRAgen is missing a default parameter. ")
      println(s"Failed with error:\n${x.getMessage()}")
      throw new Exception("missing parameter")
    case x: NumberFormatException =>
      print("[ERROR] Failed to convert parameter to its proper type with ")
      println(s"error:\n${x.getMessage()}")
      throw new Exception("failed to convert parameter")
  }
}
