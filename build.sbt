organization := "com.phasmidsoftware"

name := "TableParser"

version := "1.0.14"

scalaVersion := "2.13.6"

scalacOptions += "-deprecation"

Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.1.1"
lazy val scalaParserCombinatorsVersion = "1.1.2"
lazy val nScalaTimeVersion = "2.24.0"

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.3.5",
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
  "com.github.nscala-time" %% "nscala-time" % nScalaTimeVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

