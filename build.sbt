organization := "com.phasmidsoftware"

name := "TableParser"

version := "1.0.9"

scalaVersion := "2.13.1"

scalacOptions += "-deprecation"

Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.1.1"
lazy val scalaParserCombinatorsVersion = "1.1.2"
lazy val nScalaTimeVersion = "2.22.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "com.github.nscala-time" %% "nscala-time" % nScalaTimeVersion,
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion
)

// publishTo := sonatypePublishToBundle.value
