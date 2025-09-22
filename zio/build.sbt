name := "tableparser-zio"

//Compile / doc / scalacOptions ++= Seq("-implicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

lazy val scalaModules = "org.scala-lang.modules"
lazy val scalaTestVersion = "3.2.19"

lazy val zioVersion = "2.1.20"
lazy val zioTestVersion = "2.1.20"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "tableparser-core" % version.value,
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-http" % "3.4.0",
  "dev.zio" %% "zio-test-junit" % zioTestVersion,
  "dev.zio" %% "zio-test" % zioTestVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioTestVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioTestVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % "test",
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

