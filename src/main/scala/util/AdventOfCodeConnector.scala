package util

import sttp.client3._

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using


object AdventOfCodeConnector {

  private val baseUrl = "https://adventofcode.com/2021/day"
  private val token = sys.env.getOrElse("token", throw new Exception("No token!"))

  private def downloadFile(day: String): Unit = {
    val connectionString = s"$baseUrl/$day/input"

    val request = basicRequest
      .cookie("session", token)
      .get(uri"$connectionString")
    val backend = HttpClientSyncBackend()
    val response = request.send(backend)
    response.body match {
      case Right(x) =>
        val path = Paths.get(s"data/day$day")
        Files.write(path, x.getBytes(StandardCharsets.UTF_8))
      case Left(x) =>
        throw new Exception(x)
    }
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

  def submitAnswer(day: Number, level: Number, answer: Number) = {
    val url = s"$baseUrl/$day/answer"
    val request = basicRequest
      .cookie("session", token)
      .body(Map("level" -> s"$level", "answer" -> s"$answer"))
      .post(uri"$url")
    val backend = HttpClientSyncBackend()
    val response = request.send(backend)
    response.body match {
      case Right(x) =>
        x match {
          case _ if x.contains("That's not the right answer") =>
            println("You're wrong!")
          case _ =>
            println("You're correct!")
        }
      case Left(x) =>
        throw new Exception(x)
    }
  }
}
