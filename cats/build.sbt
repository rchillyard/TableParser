name := "tableparser-cats"

//Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.19"

lazy val nScalaTimeVersion = "2.32.0"
lazy val tsecVersion = "0.5.0"

libraryDependencies ++= Seq(
  "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion,
  "com.phasmidsoftware" %% "tableparser-core" % version.value,
  "org.typelevel" %% "cats-effect" % "3.5.7",   // This is temporary
  "org.slf4j" % "slf4j-simple" % "2.0.16" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

