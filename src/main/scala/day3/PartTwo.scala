package day3

import util.FileReader

import scala.io.Source

object PartTwo {

  def main(args: Array[String]): Unit = {
    val items = FileReader("/Users/derickfan/Desktop/workspace/advent_of_code/src/main/scala/day3/data").getLines()
    val groups = items.grouped(3).map((triple: Seq[String]) => {
      val item = triple match {
        case Seq(t1, t2, t3) =>
          t1.filter(letter => t2.contains(letter) && t3.contains(letter))
      }
      item.codePointAt(0) match {
        case value if value < 91 => value - 38
        case value if value > 91 => value - 96
      }
    })
    val sum = groups.foldLeft(0)((sum, currentValue) => sum + currentValue)
    println(sum)
  }
}
