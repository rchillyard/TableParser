name := "tableparser-core"

scalaVersion := "2.13.16"

//Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.19"

// NOTE: Issue #44: this library is not currently compatible with version 2.x.x of the parser-combinators library
lazy val scalaParserCombinatorsVersion = "2.4.0"
lazy val nScalaTimeVersion = "2.32.0"
lazy val tsecVersion = "0.4.0"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "flog" % "1.0.8",
  "io.spray" %%  "spray-json" % "1.3.6",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
//noinspection SbtDependencyVersionInspection
  scalaModules %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
  "com.github.nscala-time" %% "nscala-time" % nScalaTimeVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.16" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "org.slf4j" % "slf4j-simple" % "2.0.16" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

