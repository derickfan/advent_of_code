import util.AdventOfCodeConnector

import scala.util.Try

case class Day6() extends Problem[Number] {
  override def getData(): Seq[String] = AdventOfCodeConnector.getData("6")

  case class UniqueString(message: String = "", index: Int = -1)

  override def part1(): Number = {
    val packets = getData().mkString
    val packetsWithIndex = packets.zipWithIndex
    val length = 4
    val result = packetsWithIndex.foldLeft(UniqueString())(reduce(_, _, length))
    result.index
  }

  override def part2(): Number = {
    val packets = getData().mkString
    val packetsWithIndex = packets.zipWithIndex
    val length = 14
    val result = packetsWithIndex.foldLeft(UniqueString())(reduce(_, _, length))
    result.index
  }

  private def reduce(accumulator: UniqueString, current: (Char, Int), length: Int): UniqueString = {
    val (char, index) = current
    val message = accumulator.message
    message match {
      case _ if message.length == length => accumulator
      case _ if message contains char =>
        UniqueString(Try(message.split(char)(1)).getOrElse("") + char, index)
      case _ => UniqueString(accumulator.message + char, index + 1)
    }
  }
}
