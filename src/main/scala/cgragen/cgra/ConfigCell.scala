package cgragen.cgra

/** Configuration cell
 * @param name the name of the cell
 * @param dataSize the size of the configuration
 * @param port the port to which the cell belongs
 */
private[cgragen] final class ConfigCell(val name: String) {
  /** Fields start ***********************************************************/
  var dataSize = 1
  var port: AbstractPort = null
  /** Fields end *************************************************************/

  /** Various other methods start ********************************************/
  /** Return this cell in a string */
  override def toString(): String = name
  /** Various other methods end **********************************************/
}
