organization := "com.phasmidsoftware"

name := "TableParser"

version := "1.0.9"

scalaVersion := "2.12.10"

lazy val scalaModules = "org.scala-lang.modules"

lazy val scalaParserCombinatorsVersion = "1.0.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.github.nscala-time" %% "nscala-time" % "2.22.0",
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion
)

// publishTo := sonatypePublishToBundle.value