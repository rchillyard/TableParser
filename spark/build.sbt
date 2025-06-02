name := "tableparser-spark"

//Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "tableparser-core" % version.value,
  "org.apache.spark" %% "spark-sql" % "4.0.0",
  "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

