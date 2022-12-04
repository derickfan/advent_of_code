import util.AdventOfCodeConnector

object Main {
  def main(args: Array[String]): Unit = {
    val problem = Day4()
    val answer = problem.part2()
    println(answer)
    AdventOfCodeConnector.submitAnswer(4, 2, answer)
  }

}