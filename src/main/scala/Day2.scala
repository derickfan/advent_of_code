
import util.AdventOfCodeConnector

case class Day2() extends Problem {
  override def getData(): Seq[String] = AdventOfCodeConnector.getData("2")

  override def part1(): Number = {
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
    val games = getData()
    val points = games.foldLeft(0)((sum, currentValue) => {
      sum + legend(currentValue)
    })
    points
  }

  override def part2(): Number = {
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

    val games = getData()
    val points = games.foldLeft(0)((sum, currentValue) => {
      val array = currentValue.split(" ")
      array.reverse.head match {
        case "X" => lose(array(0)) + sum
        case "Y" => draw(array(0)) + sum
        case "Z" => win(array(0)) + sum
      }
    })
    points

  }
}

