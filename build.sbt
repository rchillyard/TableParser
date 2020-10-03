organization := "com.phasmidsoftware"

name := "TableParser"

version := "1.0.10"

scalaVersion := "2.13.3"

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
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

// publishTo := sonatypePublishToBundle.value
