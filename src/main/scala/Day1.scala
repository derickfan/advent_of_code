import util.AdventOfCodeConnector

case class Day1() extends Problem[Number] {

  override def getData(): Seq[String] = {
    AdventOfCodeConnector.getData("1")
  }

  override def part1(): Number = {
    val data = getData()
    val elfData = getElfData(data)

    val calorieSums = elfData.map((calories: List[String]) =>
      calories.foldLeft(0)((sum, currentValue) => sum + currentValue.toInt)
    )

    calorieSums.max
  }

  override def part2(): Number = {
    val data = getData()
    val elfData = getElfData(data)

    val calorieSums = elfData.map((calories: List[String]) =>
      calories.foldLeft(0)((sum, currentValue) => sum + currentValue.toInt)
    )

    calorieSums.sorted.reverse match {
      case first :: second :: third :: _ => first + second + third
      case _ => 0
    }
  }

  private def getElfData(data: Seq[String]) = {
    data.foldLeft(List[List[String]]())((accumulator, currentValue) => {
      val currentList: List[String] = currentValue match {
        case "" => List[String]()
        case _ => accumulator.headOption.map(_.appended(currentValue)).getOrElse(List(currentValue))
      }
      accumulator match {
        case _ :: tail => List(currentList) ++ accumulator
        case _ => List(currentList)
      }
    })
  }

}
