import chiseltest.VerilatorBackendAnnotation
import chiseltest.internal.NoThreadingAnnotation

import scala.collection.mutable

import scala.sys.process._

package object cgragen {
  /** Path to properties file */
  private[cgragen] final val propFile = "./src/main/resources/cgragen.properties"

  /** Parse the configuration file and return its contents in a map */
  private[cgragen] def getConfParams(): Map[String, String] = {
    val params = mutable.HashMap.empty[String, String]
    try {
      // Fetch the configuration file, parse it into a Properties object, and 
      // put all of its entries into a map
      val properties = new java.util.Properties()
      properties.load(new java.io.FileReader(propFile))
      properties.entrySet.forEach { entry =>
        params(entry.getKey.toString) = entry.getValue.toString
      }
    } catch {
      case x: Throwable =>
        println(s"Error! Failed to parse configuration file with error: ${x.getMessage()}")
        throw new Exception("failed to parse configuration")
    }
    params.toMap
  }

  /** Copy a source file to a destination file
   * @param src the path to the source file
   * @param dst the path to the destination file
   */
  private[cgragen] def copyFile(src: String, dst: String): Unit = {
    val isChannel = new java.io.FileInputStream(new java.io.File(src)).getChannel()
    val osChannel = new java.io.FileOutputStream(new java.io.File(dst)).getChannel()
    osChannel.transferFrom(isChannel, 0, Long.MaxValue)
  }

  /** Simple name generation API
   * @param init the desired named
   * @param coll the collection of names to fit `init` into
   * @return a variation on `init` unique in `coll`
   */
  private[cgragen] def genUniqueName(init: String, coll: Iterable[String]): String = {
    def loop(str: String): LazyList[String] = str #:: loop(s"${str}_")
    loop(init)
      .collectFirst { case name if !coll.exists(_ == name) => name }
      .get
  }

  /** Cache some information about the OS environment */
  private[cgragen] object OSUtils {
    private val _name = System.getProperty("os.name")

    def name = _name

    def isWindows = _name.startsWith("Windows")

    def isUnix = !isWindows
  }

  /** List of annotations to use in ChiselTest tests */
  private[cgragen] final lazy val chiselTestAnnos = {
    val basis = Seq(NoThreadingAnnotation)
    val vrltr = if (OSUtils.isWindows && "where verilator".! == 0)
      Seq(VerilatorBackendAnnotation)
    else if (OSUtils.isUnix && "which verilator".! == 0)
      Seq(VerilatorBackendAnnotation)
    else Seq()
    basis ++ vrltr
  }
}
