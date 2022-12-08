import util.AdventOfCodeConnector

object Main {
  def main(args: Array[String]): Unit = {
    val problem = Day8()
    val answer = problem.part2()
    println(s"The answer is $answer")
    AdventOfCodeConnector.submitAnswer(8,2, answer)
  }

}