package day2

import util.FileReader

import scala.io.Source

object PartOne {

  /*
  A - Rock
  B - Paper
  C - Scissors
  X - Rock - 1
  Y - Paper - 2
  Z - Scissors - 3
   */
  val legend = Map(
    "A X" -> 4,
    "B X" -> 1,
    "C X" -> 7,

    "A Y" -> 8,
    "B Y" -> 5,
    "C Y" -> 2,

    "A Z" -> 3,
    "B Z" -> 9,
    "C Z" -> 6,
  )

  def main(args: Array[String]): Unit = {
    val games = FileReader("/Users/derickfan/Desktop/workspace/advent_of_code/src/main/scala/day2/data").getLines()
    val points = games.foldLeft(0)((sum, currentValue) => {
      sum + legend(currentValue)
    })
    println(points)
  }
}
