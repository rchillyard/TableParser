ThisBuild / organization := "com.phasmidsoftware"
ThisBuild / version := "1.5.0"
ThisBuild / scalaVersion := "2.13.17"
ThisBuild / scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation")
ThisBuild / scalacOptions ++= Seq("-java-output-version", "17")
ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")

// Enforce Java 17 minimum
ThisBuild / initialize := {
  val _ = initialize.value
  val required = "17"
  val current = sys.props("java.specification.version")
  assert(
    current.toDouble >= required.toDouble,
    s"Java $required or higher required. Current version: $current"
  )
}

ThisBuild / resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

// Shared versions
val scalaTestVersion = "3.2.19"
val scalaParserCombinatorsVersion = "2.4.0"
val nScalaTimeVersion = "3.0.0"
val tsecVersion_core = "0.4.0"
val tsecVersion_cats = "0.5.0"
val catsVersion = "3.6.3" // NOTE 3.7.0 appears to be missing the encryption classes that we need.
val zioVersion = "2.1.22" // NOTE 2.1.24 is broken for some reason.
val zioHttpVersion = "3.4.0" // NOTE 3.10.1 causes a dependency conflict.
val sparkVersion = "4.0.1"

lazy val core = project.settings(
  name := "tableparser-core",
  libraryDependencies ++= Seq(
    "com.phasmidsoftware" %% "flog" % "1.0.10",
    "io.spray" %% "spray-json" % "1.3.6",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
    "com.github.nscala-time" %% "nscala-time" % nScalaTimeVersion,
    "ch.qos.logback" % "logback-classic" % "1.5.32" % Runtime,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val cats = project.dependsOn(core).settings(
  name := "tableparser-cats",
  libraryDependencies ++= Seq(
    "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion_cats,
    "org.typelevel" %% "cats-effect" % catsVersion,
    "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val parquet = project.dependsOn(core).settings(
name := "tableparser-parquet",
  libraryDependencies ++= Seq(
    "org.apache.parquet" % "parquet-column" % "1.15.2",
    "org.apache.parquet" % "parquet-hadoop" % "1.15.2",
    "org.apache.hadoop"  % "hadoop-common"  % "3.4.1" % "provided",
    "org.apache.hadoop"  % "hadoop-mapreduce-client-core" % "3.4.1" % Test,
    "org.scalatest"     %% "scalatest"      % scalaTestVersion % Test
  )
)

lazy val spark = project.dependsOn(core).settings(
  name := "tableparser-spark",
  libraryDependencies ++= Seq(
    "org.apache.spark" %% "spark-sql" % sparkVersion % "provided",
    "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val zio = project.dependsOn(core).settings(
  name := "tableparser-zio",
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-http" % zioHttpVersion,
    "dev.zio" %% "zio-test-junit" % zioVersion,
    "dev.zio" %% "zio-test" % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
    "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val root = (project in file("."))
        .aggregate(core, cats, parquet, spark, zio)
        .settings(
          name := "TableParser",
          publish / skip := true
        )

Test / parallelExecution := false

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"