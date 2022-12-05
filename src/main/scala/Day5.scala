import util.AdventOfCodeConnector

case class Day5() extends Problem[String] {
  override def getData(): Seq[String] = AdventOfCodeConnector.getData("5")

  case class Column(blocks: Seq[Char] = Seq())

  private val stacks = {
    val a = for {
      _ <- 0 until 10
    } yield {
      Column(Seq())
    }
    a.zipWithIndex.map(a => (a._2, a._1)).toMap
  }

  override def part1(): String = {
    val data = getData()
    val (chart, instructions) = data.splitAt(data.indexOf(""))
    val chartWithIndex = chart.map(_.zipWithIndex)
    val initialStack = getInitialStack(chartWithIndex)

    val endStack = instructions.tail.foldLeft(initialStack){ (accumulator, instruction) =>
      val (amount, from ,to) = getValues(instruction)
      val fromBlocks = accumulator(from).blocks.dropRight(amount)
      val movingBlocks = accumulator(from).blocks.slice(fromBlocks.length, fromBlocks.length + amount)
      val toBlocks = accumulator(to).blocks ++ movingBlocks.reverse
      accumulator + (
        from -> accumulator(from).copy(blocks = fromBlocks),
        to -> accumulator(to).copy(blocks = toBlocks)
      )
    }

    getTopRow(endStack)
  }

  override def part2(): String = {
    val data = getData()
    val (chart, instructions) = data.splitAt(data.indexOf(""))
    val chartWithIndex = chart.map(_.zipWithIndex)
    val initialStack = getInitialStack(chartWithIndex)

    val endStack = instructions.tail.foldLeft(initialStack) { (accumulator, instruction) =>
      val (amount, from, to) = getValues(instruction)
      val fromBlocks = accumulator(from).blocks.dropRight(amount)
      val movingBlocks = accumulator(from).blocks.slice(fromBlocks.length, fromBlocks.length + amount)
      val toBlocks = accumulator(to).blocks ++ movingBlocks
      accumulator + (
        from -> accumulator(from).copy(blocks = fromBlocks),
        to -> accumulator(to).copy(blocks = toBlocks)
      )
    }

    getTopRow(endStack)
  }

  private def getTopRow(endStack: Map[Int, Column]) = {
    endStack.toList.sortWith((a, b) => a._1 < b._1).map(_._2.blocks.lastOption.getOrElse("")).mkString
  }

  private def getInitialStack(chartWithIndex: Seq[IndexedSeq[(Char, Int)]]) = {
    chartWithIndex.foldLeft(stacks) { (accumulator, row) =>
      val blocks = row.filter(a => a._1.isLetter).map((a) => (a._1, (a._2 - 1) / 4))
      blocks.foldLeft(accumulator) { (stack, crate) => {
        val idx = crate._2
        val temp = stack(idx)
        stack + (idx -> temp.copy(crate._1 +: temp.blocks))
      }
      }
    }
  }

  private def getValues(instruction: String): (Int, Int, Int) = {
    val nums = instruction.split(" ").toList
    val amount = nums(1).toInt
    val from = nums(3).toInt - 1
    val to = nums(5).toInt - 1
    (amount, from, to)
  }
}

