package util

import java.io.File
import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

object FileDownloader {

  private val baseUrl = "https://adventofcode.com/2022/day"
  private def downloadFile(day: String): Unit = {
    val token = sys.env.getOrElse("token", throw new Exception("No token!"))

    val connectionString = s"$baseUrl/$day/input"
    val url = new URL(connectionString)

    val connection: HttpURLConnection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestProperty("Cookie", token)

    val inputStream = connection.getInputStream
    val path = Paths.get(s"data/day$day")
    Files.copy(inputStream, path)
  }

  def getData(day: String): Seq[String] = {
    val file = new File(s"data/day$day")
    if (file.exists()) {
      Using(Source.fromFile(file)) { a => a.getLines().toList }.getOrElse(throw new Exception())
    } else {
      downloadFile(day)
      Using(Source.fromFile(file)) { a => a.getLines().toList }.getOrElse(throw new Exception())
    }
  }


}
