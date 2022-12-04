import util.AdventOfCodeConnector

object Main {
  def main(args: Array[String]): Unit = {
    val problem = Day3()
    val answer = problem.part2()
    println(answer)
    AdventOfCodeConnector.submitAnswer(3, 2, answer)
  }

}