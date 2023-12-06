import scala.collection.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2023"
  )

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "os-lib" % "0.9.2",
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)
