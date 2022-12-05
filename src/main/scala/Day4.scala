import util.AdventOfCodeConnector


case class Day4() extends Problem[Number] {
  case class AssignmentPairs(left: Int, right: Int)


  override def getData(): Seq[String] = {
    AdventOfCodeConnector.getData("4")
  }

  override def part1(): Number = {
    val data: Seq[String] = getData()
    val pairs = data.map(line => {
      line.replace("-", ",").split(",").toList match {
        case Seq(firstLeft, firstRight, secondLeft, secondRight) =>
          (AssignmentPairs(firstLeft.toInt, firstRight.toInt), AssignmentPairs(secondLeft.toInt, secondRight.toInt))
      }
    })

    def check(a: AssignmentPairs, b: AssignmentPairs) = a.left <= b.left && a.right >= b.right

    val filteredPairs = pairs.filter((p) => {
      val first = p._1
      val second = p._2
      check(first, second) || check(second, first)
    })
    filteredPairs.size
  }


  override def part2(): Number = {
    val data: Seq[String] = getData()
    val pairs = data.map(line => {
      line.replace("-", ",").split(",").toList match {
        case Seq(firstLeft, firstRight, secondLeft, secondRight) =>
          (AssignmentPairs(firstLeft.toInt, firstRight.toInt), AssignmentPairs(secondLeft.toInt, secondRight.toInt))
      }
    })

    def check(a: AssignmentPairs, b: AssignmentPairs) = a.left <= b.left && b.left <= a.right

    val filteredPairs = pairs.filter((p) => {
      val first = p._1
      val second = p._2
      check(first, second) || check(second, first)
    })
    filteredPairs.size
  }
}
