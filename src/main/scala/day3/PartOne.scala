package day3

import util.FileReader

import scala.io.Source

object PartOne {

  def main(args: Array[String]): Unit = {
    val items = FileReader("/Users/derickfan/Desktop/workspace/advent_of_code/src/main/scala/day3/data").getLines()
    val values = items.toList.map((i: String) => {
      val mid = i.size / 2
      val item1 = i.substring(0, mid)
      val item2 = i.substring(mid, i.size)
      val commonItem = item1.filter(item2.contains(_))
      commonItem.codePointAt(0) match {
        case value if value < 91 => value - 38
        case value if value > 91 => value - 96
      }
    })

    val sum = values.foldLeft(0)((sum, currentValue) => sum + currentValue)
    println(sum)

  }
}
