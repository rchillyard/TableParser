name := "tableparser-cats"

//Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.19"

lazy val tsecVersion = "0.5.0"
lazy val catsVersion = "3.6.1"

libraryDependencies ++= Seq(
  "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion,
  "com.phasmidsoftware" %% "tableparser-core" % version.value,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

