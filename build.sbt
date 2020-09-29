organization := "com.phasmidsoftware"

name := "TableParser"

version := "1.0.10-SNAPSHOT"

scalaVersion := "2.13.3"

scalacOptions := Seq("-unchecked", "-deprecation")

lazy val scalaModules = "org.scala-lang.modules"

lazy val scalaParserCombinatorsVersion = "1.1.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "com.github.nscala-time" %% "nscala-time" % "2.22.0",
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion
)

// publishTo := sonatypePublishToBundle.value