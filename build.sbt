name := "CsvParser"

version := "1.0"

scalaVersion := "2.12.6"

lazy val scalaModules = "org.scala-lang.modules"

lazy val scalaParserCombinatorsVersion = "1.0.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
//  "io.spray" %%  "spray-json" % "1.3.3",
  "com.github.nscala-time" %% "nscala-time" % "2.22.0",
//"joda-time" % "joda-time" % "2.9.9",
//  "junit" % "junit" % "4.12" % "test",
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion
)