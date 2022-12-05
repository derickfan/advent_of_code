import util.AdventOfCodeConnector

object Main {
  def main(args: Array[String]): Unit = {
    val problem = Day5()
    val answer = problem.part2()
    println(answer)
    AdventOfCodeConnector.submitAnswer(5, 2, answer)
  }

}