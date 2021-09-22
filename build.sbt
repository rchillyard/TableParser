organization := "com.phasmidsoftware"

name := "TableParser"

version := "1.0.14"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.9"
lazy val scalaParserCombinatorsVersion = "1.1.2"
lazy val nScalaTimeVersion = "2.28.0"

libraryDependencies ++= Seq(
//  "com.phasmidsoftware" %% "flog" % "1.0.8",
  "io.spray" %%  "spray-json" % "1.3.6",
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
  "com.github.nscala-time" %% "nscala-time" % nScalaTimeVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.6" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

