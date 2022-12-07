import util.AdventOfCodeConnector

import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    val problem = Day7()
    val answer = problem.part2()
    println(s"The answer is $answer")
    AdventOfCodeConnector.submitAnswer(7,2, answer)
  }

}