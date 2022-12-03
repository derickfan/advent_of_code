package day2

import scala.io.Source

object PartTwo {

  /*
  A - Rock
  B - Paper
  C - Scissors
  X - Rock - 1
  Y - Paper - 2
  Z - Scissors - 3
   */
  val draw = Map(
    "A" -> 4,
    "B" -> 5,
    "C" -> 6,
  )

  val win = Map(
    "C" -> 7,
    "A" -> 8,
    "B" -> 9,
  )

  val lose = Map(
    "B" -> 1,
    "C" -> 2,
    "A" -> 3,
  )

  val legend = Map(
    "X" -> lose,
    "Y" -> draw,
    "Z" -> win
  )

  def main(args: Array[String]): Unit = {
    val filename = "/Users/derickfan/Desktop/workspace/advent_of_code/src/main/scala/day2/data"
    val games = Source.fromFile(filename).getLines
    val points = games.foldLeft(0)((sum, currentValue) => {
      val array = currentValue.split(" ")
      array.reverse.head match {
        case "X" => lose(array(0)) + sum
        case "Y" => draw(array(0)) + sum
        case "Z" => win(array(0)) + sum
      }
    })
    println(points)
  }
}
