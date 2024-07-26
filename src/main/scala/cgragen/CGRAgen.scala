package cgragen

import cgragen.archparse.Architecture

import cgragen.bits.BitStream

import cgragen.cgra.{CGRA, AbstractModule}

import cgragen.dfgparse.DFG, DFG.isDAG

import cgragen.hwgen.{HWGen, SysGen}

import cgragen.mapping.{Mapper, Approximator}

import cgragen.mrrg.MRRG

import cgragen.opt.getOptPasses

import scala.collection.mutable

import java.io.{File, FileOutputStream, PrintWriter}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/** Main object of CGRAgen */
object CGRAgen {
  /** Main function for running the tool
   * @param args an array of string arguments
   */
  def main(args: Array[String]): Unit = {
    val usage = {
      """Usage: runMain cgragen.CGRAgen [options] arch_filename [dfg_filename]
        |Options:
        | -h, --help                        Display this information.
        | -debug                            Print all debug messages.
        | -gen-hw                           Generate a hardware description of the passed architecture.
        | -approx-arith                     Apply approximate arithmetic during the mapping procedure.
        | -infer-io                         Infer top-level IO for the passed architecture.
        | -serial                           Use serial configuration in the passed architecture.
        | -remove-dead                      Remove dead nodes and edges from the passed DFG.
        | -prop-const                       Propagate constants in the passed DFG. Also enables dead node removal.
        | -repl-mc-const                    Replicate constants in the passed DFG to avoid multi-casting.
        | -limit-fanout [<arg>]             Duplicate nodes in the DFG to limit the maximum fanout.
        | -dataSize <arg>                   Default data size of all architecture internals.
        | -ii <arg>                         Maximum initiation interval of the architecture.
        | -print-dfg                        Print the parsed (and optimized) DFG in DOT format to stdout.
        | -print-arch                       Print the parsed (and built) architecture in DOT format to stdout.
        | -m, --mapper                      Choose which mapper to use (one of ["heuristic"], defaults to "heuristic").
        | -m-arg, --mapper-arg <key=value>  Pass an argument to the mapper.
        |This guide assumes the use of SBT to build CGRAgen.""".stripMargin
    }

    // Parse the arguments
    val passedArgMap = try {
      // Recursively run through arguments
      val keyValueRegex = "([a-zA-Z].*)=(\\S+)".r
      def isSwitch(s: String) = s.startsWith("-")
      def listToArgMap(ls: List[String]): Map[String, String] = {
        ls match {
          case arch :: dfg :: tail if (!isSwitch(arch) && !isSwitch(dfg)) =>
            Map("archFile" -> arch, "dfgFile" -> dfg) ++ listToArgMap(tail)
          case arch :: tail if !isSwitch(arch) =>
            Map("archFile" -> arch) ++ listToArgMap(tail)
          case "-h" :: tail =>
            Map("help" -> "") ++ listToArgMap(tail)
          case "--help" :: tail =>
            Map("help" -> "") ++ listToArgMap(tail)
          case "-debug" :: tail =>
            Map("cgra.DebugPrints" -> "1", "dfg.DebugPrints" -> "1",
                "mrrg.DebugPrints" -> "1", "hw.DebugPrints" -> "1",
                "mapper.DebugPrints" -> "1") ++ listToArgMap(tail)
          case "-approx-arith" :: tail =>
            Map("cgra.ApproximateArithmetic" -> "1") ++ listToArgMap(tail)
          case "-dataSize" :: value :: tail if value.toIntOption != None =>
            Map("cgra.DataSize" -> value) ++ listToArgMap(tail)
          case "-ii" :: value :: tail if value.toIntOption != None =>
            Map("cgra.II" -> value) ++ listToArgMap(tail)
          case "-gen-hw" :: tail =>
            Map("hw.Generate" -> "1") ++ listToArgMap(tail)
          case "-infer-io" :: tail =>
            Map("cgra.InferTopLevelIO" -> "1") ++ listToArgMap(tail)
          case "-serial" :: tail =>
            Map("hw.ParallelConfiguration" -> "0") ++ listToArgMap(tail)
          case "-remove-dead" :: tail =>
            Map("dfg.RemoveDeadNodes" -> "1") ++ listToArgMap(tail)
          case "-prop-const" :: tail =>
            Map("dfg.PropagateConstants" -> "1") ++ listToArgMap(tail)
          case "-repl-mc-const" :: tail =>
            Map("dfg.AvoidMulticastConstants" -> "1") ++ listToArgMap(tail)
          case "-limit-fanout" :: value :: tail if value.toIntOption != None =>
            Map("dfg.LimitFanout" -> "1", "dfg.MaxFanout" -> value) ++ listToArgMap(tail)
          case "-limit-fanout" :: tail =>
            Map("dfg.LimitFanout" -> "1") ++ listToArgMap(tail)
          case "-print-dfg" :: tail =>
            Map("printDfg" -> "") ++ listToArgMap(tail)
          case "-print-arch" :: tail =>
            Map("printArch" -> "") ++ listToArgMap(tail)
          case "-m" :: value :: tail if !isSwitch(value) =>
            Map("mapper.Mapper" -> value) ++ listToArgMap(tail)
          case "--mapper" :: value :: tail if !isSwitch(value) =>
            Map("mapper.Mapper" -> value) ++ listToArgMap(tail)
          case "-m-arg" :: value :: tail if !isSwitch(value) =>
            value match {
              case keyValueRegex(k, v) => Map(s"mapper.any.$k" -> v) ++ listToArgMap(tail)
              case _ =>
                println(s"[INFO] Ignoring malformed key-value argument ($value) to mapper")
                listToArgMap(tail)
            }
          case "--mapper-arg" :: value :: tail if !isSwitch(value) =>
            value match {
              case keyValueRegex(k, v) => Map(s"mapper.any.$k" -> v) ++ listToArgMap(tail)
              case _ =>
                println(s"[INFO] Ignoring malformed key-value argument ($value) to mapper")
                listToArgMap(tail)
            }
          case x :: tail =>
            println(s"Warning! Invalid argument $x ignored during parsing.")
            listToArgMap(tail)
          case Nil => Map.empty[String, String]
        }
      }
      val argMap = listToArgMap(args.toList)

      // If no or too few arguments are passed, print the usage message and exit
      if (argMap.isEmpty || argMap.contains("help")) {
        println(usage)
        return
      }

      // Check that at least the architecture file was provided (if -gen-hardware was specified)
      if (!argMap.contains("archFile") || !argMap("archFile").endsWith(".xml")) {
        println("[ERROR] XML-formatted architecture file not provided")
        return
      }

      // Verify that at least the architecture file exists
      val archFile = argMap("archFile")
      if (!(new File(archFile)).exists()) {
        println(s"[ERROR] Given architecture file ($archFile) does not exist")
        return
      }
      if (argMap.contains("dfgFile")) {
        val dfgFile = argMap("dfgFile")
        if (!(new File(dfgFile)).exists()) {
          println(s"[ERROR] Given DFG file ($dfgFile) does not exist")
          return
        }
      }

      argMap
    } catch {
      case err: Throwable =>
        println("[ERROR] Failed to parse command line arguments with errors:")
        println(err.getMessage())
        return
    }

    // Parse and construct the default configuration of the tool
    val defaults = getConfParams()

    // Convert parameters into an implicit configuration
    implicit val params = Parameters(defaults ++ passedArgMap)

    // Get the current date and time to format the output file name
    val frmt = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss")
    val time = LocalDateTime.now().format(frmt)
    val folderName = s"runs/$time/"
    (new File(folderName)).mkdirs()

    // Copy the configuration file to the run folder
    val propName = propFile.split('/').last
    copyFile(propFile, s"$folderName/$propName")

    // Get the names of the architecture and DFG
    val archName = passedArgMap("archFile").split('/').last.split('.').head

    // Copy the architecture and DFG files to the run folder
    copyFile(passedArgMap("archFile"), s"$folderName/$archName.xml")

    // Parse the architecture and convert it to a CGRA
    val arch = Architecture(passedArgMap("archFile"))    
    val cgra = CGRA(arch)

    // Print the CGRA as a DOT string, if requested
    if (passedArgMap.contains("printArch")) {
      val optCgraPath = s"${folderName}/${archName}_arch.dot"
      (new PrintWriter(new File(optCgraPath)))
        .append(cgra.toDotString())
        .close()
      val optCgraMrrgPath = s"$folderName/${archName}_mrrg.dot"
      (new PrintWriter(new File(optCgraMrrgPath)))
        .append(MRRG(cgra, 1).toDotString())
        .close()
    }

    if (params.HWGenerate) {
      // Generate Verilog descriptions of all top-level module templates and 
      // the CGRA itself
      val files = mutable.ArrayBuffer.empty[(String, String)]

      arch.getTopModTemplates().foreach { case (modName, modTemp) =>
        try {
          val vString = HWGen(AbstractModule(modName, modTemp, arch))
          val vFile   = s"$folderName${archName}_${modName}.v"
          (new PrintWriter(new File(vFile))).append(vString).close()
          files += ((modName, vFile))
        } catch {
          case x: Throwable =>
            print("[ERROR] Failed to generate hardware description of module ")
            println(s"template ($modName) with errors:\n${x.getMessage()}")
        }
      }

      if (cgra.subModules.nonEmpty) {
        val cgraSuccess = try {
          val cgraVString = HWGen(cgra)
          val cgraVFile   = s"$folderName${archName}.v"
          (new PrintWriter(new File(cgraVFile))).append(cgraVString).close()
          files += ((archName, cgraVFile))
          true
        } catch {
          case x: Throwable =>
            print("[ERROR] Failed to generate hardware description of CGRA ")
            println(s"with errors:\n${x}")
            false
        }

        if (cgraSuccess) try {
          val cgraSysVString = SysGen(cgra)
          val sysName = genUniqueName("sys", arch.getTopModTemplates().map(_._1))
          val cgraSysVFile = s"$folderName${archName}_${sysName}.v"
          (new PrintWriter(new File(cgraSysVFile))).append(cgraSysVString).close()
          files += ((s"${archName}_system", cgraSysVFile))
        } catch {
          case x: Throwable =>
            print("[ERROR] Failed to generate hardware description of CGRA ")
            println(s"system with errors:\n${x.getMessage()}")
        }
      }

      val padSize = files.map(_._1.size).max
      print(s"Printed Verilog code for the top-level module templates ")
      println(s"${if (cgra.subModules.nonEmpty) "and the CGRA " else ""}to files:")
      files.foreach { case (name, path) => println(s" - ${name.padTo(padSize, ' ')} : $path") }
    }

    // The existence of a DFG file determines whether to proceed here
    if (passedArgMap.contains("dfgFile")) {
      // Get the name of the DFG
      val dfgName = passedArgMap("dfgFile").split('/').last.split('.').head

      // Copy the DFG file to the run folder
      copyFile(passedArgMap("dfgFile"), s"$folderName/$dfgName.dot")

      // Parse the DFG and pass it through any number of optimization passes 
      // currently implemented and activated in the tool options
      val dfg = DFG(passedArgMap("dfgFile"))

      // Compile the list of "optimizations" to apply to the DFG given the 
      // target architecture
      val opt = getOptPasses()
      opt(dfg, cgra)

      // Print the updated DFG as a DOT string, if requested
      if (passedArgMap.contains("printDfg")) {
        val optDfgPath = s"$folderName/${dfgName}_opt.dot"
        (new PrintWriter(new File(optDfgPath)))
          .append(dfg.toString())
          .close()
      }

      // Now, run the mapping tool on the DFG, if it is a DAG
      println("Running mapper on the given CGRA and DFG")
      if (!params.HWParallelConfiguration && params.II > 1) {
        print("Beware that DFGs mapped to architectures with serial ")
        print("configuration for II>1 will suffer from poor performance. ")
        println("Consider changing your architecture or mapper configuration.")
      }
      if (!DFG.isDAG(dfg)) {
        println("[ERROR] Cannot perform mapping for DFGs with backward edges")
        return
      }
      val mapper  = Mapper(cgra)
      val mapping = mapper.mapGraph(dfg).getOrElse {
        print("[ERROR] Failed to map DFG to CGRA. Check your configuration, ")
        println("architecture, and DFG, then re-run the tool")
        return
      }

      // Finally, apply the approximator, if requested
      if (mapping.isApproximable) {
        println("Running approximator on the mapping")
        val apprxmtr = Approximator()
        apprxmtr.apprxMapping(mapping)
      }

      // Output the mapping to a file
      val mapFile = s"$folderName${archName}_${dfgName}.map"
      val mapPW   = new PrintWriter(new File(mapFile))
      println(s"Successfully mapped DFG to CGRA. Mapping printed to file ($mapFile)")
      mapPW.append(mapping.toString()).close()

      // Output the configuration bit-stream to a text file
      println("Generating configuration from mapping")
      val bitStream = BitStream(mapping)
      val bsFile    = s"$folderName${archName}_${dfgName}.bs"
      val bsPW      = new PrintWriter(new File(bsFile))
      bsPW.append(bitStream.toString()).close()
      println(s"Textual configuration printed to file ($bsFile)")

      val bits    = bitStream.toBinary()
      val bitFile = s"$folderName${archName}_${dfgName}.bit"
      val bitFW   = new FileOutputStream(new File(bitFile))
      bitFW.write(bits, 0, bits.size)
      bitFW.close()
      println(s"Binary configuration printed to file ($bitFile)")
    }
  }
}
