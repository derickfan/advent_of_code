import util.AdventOfCodeConnector

import scala.annotation.tailrec

case class Day7() extends Problem[Long] {
  override def getData(): Seq[String] = AdventOfCodeConnector.getData("7")

  case class FileSystem(currentPath: List[String] = List(), folders: Map[String, Long] = Map(), deletableSize: Long = 0L)

  private def createFileSystem(): FileSystem = {
    val data = getData()
    val fileSystem = data.foldLeft(FileSystem()) {
      (fileSystem, current) => {
        val currentPath = fileSystem.currentPath
        val folders = fileSystem.folders
        val deletableSize = fileSystem.deletableSize
        val currentSize = folders.getOrElse(currentPath.mkString, 0L)
        current.split(" ").toList match {
          // If going up 1 level we should add the current folders size to the parents folder
          case Seq("$", "cd", "..") =>
            fileSystem.copy(
              currentPath = currentPath.dropRight(1),
              deletableSize = deletableSize + (if (currentSize <= 100000) currentSize else 0L))
          // Update current path
          case Seq("$", "cd", path) =>
            fileSystem.copy(currentPath = currentPath ++ List(path))
          // If file adds the size to the folder and parents
          case Seq(size, _) if size.forall(_.isDigit) =>
            updateFolderSize(fileSystem, currentPath, size.toLong)
          // Ignore dirs and ls
          case _ =>
            fileSystem
        }
      }
    }
    fileSystem
  }

  @tailrec
  private def updateFolderSize(fileSystem: FileSystem, currentPath: Seq[String], size: Long): FileSystem = {
    currentPath match {
      case Seq() => fileSystem
      case _ =>
        val currentPathString = currentPath.mkString
        val updatedFileSystem = fileSystem.copy(
          folders = fileSystem.folders +
            (currentPathString -> (fileSystem.folders.getOrElse(currentPathString, 0L) + size) )
        )
        updateFolderSize(updatedFileSystem, currentPath.dropRight(1), size)
    }
  }
  override def part1(): Long = {
    val fileSystem = createFileSystem()
    fileSystem.deletableSize
  }

  override def part2(): Long = {
    val fileSystem = createFileSystem()
    val sortedFiles = fileSystem.folders.toList.sortWith((a, b) => a._2 < b._2)
    val needed = 30000000L
    val currentCapacity = fileSystem.folders.getOrElse("/", 0L)
    sortedFiles.find(
      folder => currentCapacity - folder._2  < needed
    ).map(_._2).getOrElse(0L)
  }
}
