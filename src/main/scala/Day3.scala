import util.AdventOfCodeConnector

case class Day3() extends Problem[Number] {
  override def getData(): Seq[String] = AdventOfCodeConnector.getData("3")

  override def part1(): Number = {
    val items = getData()
    val values = items.toList.map((i: String) => {
      val mid = i.size / 2
      val item1 = i.substring(0, mid)
      val item2 = i.substring(mid, i.size)
      val commonItem = item1.filter(item2.contains(_))
      commonItem.codePointAt(0) match {
        case value if value < 91 => value - 38
        case value if value > 91 => value - 96
      }
    })

    val sum = values.foldLeft(0)((sum, currentValue) => sum + currentValue)
    sum
  }

  override def part2(): Number = {
    val items = getData()
    val groups = items.grouped(3).map((triple: Seq[String]) => {
      val item = triple match {
        case Seq(t1, t2, t3) =>
          t1.filter(letter => t2.contains(letter) && t3.contains(letter))
      }
      item.codePointAt(0) match {
        case value if value < 91 => value - 38
        case value if value > 91 => value - 96
      }
    })
    val sum = groups.foldLeft(0)((sum, currentValue) => sum + currentValue)
    sum
  }
}

