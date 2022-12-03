package day1

import util.FileReader

import scala.io.Source

object PartTwo {

  def main(args: Array[String]): Unit = {
    val lineIterator = FileReader("/Users/derickfan/Desktop/workspace/advent_of_code/src/main/scala/day1/data").getLines()
    val elfData = lineIterator.foldLeft(List[List[String]]())((accumulator, currentValue) => {
      val currentList: List[String] = currentValue match {
        case "" => List[String]()
        case _ => accumulator.headOption.map(_.appended(currentValue)).getOrElse(List(currentValue))
      }
      accumulator match {
        case _ :: tail => List(currentList) ++ accumulator
        case _ => List(currentList)
      }
    })

    val calorieSums = elfData.map((calories: List[String]) =>
      calories.foldLeft(0)((sum, currentValue) => sum + currentValue.toInt)
    )

    val sumOfThree = calorieSums.sorted.reverse match {
      case first :: second :: third :: _ => first + second + third
      case _ => 0
    }
    println(sumOfThree)
  }

}
