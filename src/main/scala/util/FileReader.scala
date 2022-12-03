package util

import scala.io.Source

case class FileReader(fileLocation: String) {

  def getLines(): Seq[String] = Source.fromFile(fileLocation).getLines.toList

}
