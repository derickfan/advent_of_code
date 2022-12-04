ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "advent_of_code",
    libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.8.3"
  )
