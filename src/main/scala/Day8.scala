import util.AdventOfCodeConnector

case class Day8() extends Problem[Int] {
  override def getData(): Seq[String] = AdventOfCodeConnector.getData("8")

  case class Tree(height: Int, position: Position)

  case class Position(y: Int, x: Int)

  private val gridSize = 98

  override def part1(): Int = {
    def check(currentPosition: Position, height: Int, forest: Map[Position, Int])(func: (Position, (Position, Int)) => Boolean): Boolean = {
      forest.filter(tree => func(currentPosition, tree)).forall(tree =>
        checkHeight(height, forest, tree)
      )
    }

    def checkLeft(currentPosition: Position, tree: (Position, Int)): Boolean = {
      val (position, _) = tree
      position.y < currentPosition.y && position.x == currentPosition.x
    }

    def checkRight(currentPosition: Position, tree: (Position, Int)): Boolean = {
      val (position, _) = tree
      position.y > currentPosition.y && position.x == currentPosition.x
    }

    def checkUp(currentPosition: Position, tree: (Position, Int)): Boolean = {
      val (position, _) = tree
      position.x > currentPosition.x && position.y == currentPosition.y
    }

    def checkDown(currentPosition: Position, tree: (Position, Int)): Boolean = {
      val (position, _) = tree
      position.x < currentPosition.x && position.y == currentPosition.y
    }

    val forest: Map[Position, Int] = createForest

    val visibleTrees = forest.filter(tree => {
      val (position, height) = tree
      position match {
        case Position(0, _) |  Position(_, 0) => true
        case p if p.x == gridSize || p.y == gridSize => true
        case Position(x, y) =>
          check(position, height, forest)(checkLeft) ||
            check(position, height, forest)(checkUp) ||
            check(position, height, forest)(checkDown) ||
            check(position, height, forest)(checkRight)
      }
    })

    visibleTrees.size
  }


  private def createForest: Map[Position, Int] = {
    val trees: Seq[Tree] = getData().map(_.split("").zipWithIndex.toList).zipWithIndex.toList.flatMap {
      row: (List[(String, Int)], Int) =>
        row._1.map { tree =>
          Tree(
            height = tree._1.toInt,
            position = Position(row._2, tree._2)
          )
        }
    }

    val forest: Map[Position, Int] = trees.foldLeft(Map[Position, Int]()) { (accumulator, tree) =>
      accumulator + (tree.position -> tree.height)
    }
    forest
  }

  private def checkHeight(height: Int, forest: Map[Position, Int], tree: (Position, Int)) = {
    forest.get(tree._1).map(_ < height).getOrElse(true)
  }

  override def part2(): Int = {

    def checkLeft(currentPosition: Position, height: Int, forest: Map[Position, Int]): Int = {
      val result = forest.filter(tree => {
        val (position, height) = tree
        position.y < currentPosition.y && position.x == currentPosition.x
      })
      val sorted = result.toList.sortWith((a, b) => a._1.y > b._1.y)

      sorted.find(a => a._2 >= height)
        .map(block => currentPosition.y - block._1.y)
        .getOrElse(currentPosition.y)
    }

    def checkRight(currentPosition: Position, height: Int, forest: Map[Position, Int]): Int = {
      val result = forest.filter(tree => {
        val (position, height) = tree
        position.y > currentPosition.y && position.x == currentPosition.x
      })
      val sorted = result.toList.sortWith((a, b) => a._1.y < b._1.y)

      sorted.find(a => a._2 >= height)
        .map(block => block._1.y - currentPosition.y)
        .getOrElse(gridSize - currentPosition.y)
    }

    def checkUp(currentPosition: Position, height: Int, forest: Map[Position, Int]): Int = {
      val result = forest.filter(tree => {
        val (position, height) = tree
        position.x < currentPosition.x && position.y == currentPosition.y
      })
      val sorted = result.toList.sortWith((a, b) => a._1.x > b._1.x)

      sorted.find(a => a._2 >= height)
        .map(block => currentPosition.x - block._1.x)
        .getOrElse(currentPosition.x)
    }

    def checkDown(currentPosition: Position, height: Int, forest: Map[Position, Int]): Int = {
      val result = forest.filter(tree => {
        val (position, height) = tree
        position.x > currentPosition.x && position.y == currentPosition.y
      })
      val sorted = result.toList.sortWith((a, b) => a._1.x < b._1.x)

      sorted.find(a => a._2 >= height)
        .map(block => block._1.x - currentPosition.x)
        .getOrElse(gridSize - currentPosition.x)
    }

    val forest: Map[Position, Int] = createForest

    val result = forest.map(tree => {
      val (currentPosition, height) = tree
      val left = checkLeft(currentPosition, height, forest)
      val right = checkRight(currentPosition, height, forest)
      val up = checkUp(currentPosition, height, forest)
      val down = checkDown(currentPosition, height, forest)
      left * right * up * down
    }).toList.sortWith((a, b) => a > b).head

    result
  }
}
