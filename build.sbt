ThisBuild / organization := "com.phasmidsoftware"
ThisBuild / version := "1.7.1"

// Build Scala 3 with the oldest supported LTS so that the artifacts are usable from any later Scala 3 version.
// CircleCI also runs the tests on the current LTS (3.9).
val scala213 = "2.13.18"
val scala3 = "3.3.8"

ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-feature")
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

Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)

ThisBuild / resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

// Shared versions
val scalaTestVersion = "3.2.20"
val scalaParserCombinatorsVersion = "2.5.0"
val nScalaTimeVersion = "3.2.0"
val tsecVersion = "0.6.0"
val catsVersion = "3.7.1"
val zioVersion = "2.1.27"
val logbackVersion = "1.5.38"
val slf4jVersion = "2.0.20"
val parquetVersion = "1.18.1"
val hadoopVersion = "3.4.3"
val sparkVersion = "4.0.4"

// The examples, Main, Sequential and EvaluateZIO are deprecated in 1.7.0 and leave the library in 2.0.0.
// The tests (and spark, which stays on 1.x) still use them, so silence just those deprecation warnings there.
val silence170Deprecations = "-Wconf:cat=deprecation&msg=1\\.7\\.0:s"

lazy val core = project.settings(
  name := "tableparser-core",
  Test / scalacOptions += silence170Deprecations,
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-json" % "1.3.6",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
    "com.github.nscala-time" %% "nscala-time" % nScalaTimeVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val cats = project.dependsOn(core).settings(
  name := "tableparser-cats",
  Test / scalacOptions += silence170Deprecations,
  libraryDependencies ++= Seq(
    "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion,
    "org.typelevel" %% "cats-effect" % catsVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val parquet = project.dependsOn(core).settings(
  name := "tableparser-parquet",
  // The test resources include two ~50MB Parquet files: don't publish them in tests jars.
  Test / publishArtifact := false,
  libraryDependencies ++= Seq(
    "org.apache.parquet" % "parquet-column" % parquetVersion,
    "org.apache.parquet" % "parquet-hadoop" % parquetVersion,
    "org.apache.hadoop"  % "hadoop-common"  % hadoopVersion % "provided",
    "org.apache.hadoop"  % "hadoop-mapreduce-client-core" % hadoopVersion % Test,
    "org.scalatest"     %% "scalatest"      % scalaTestVersion % Test
  )
)

// NOTE: Spark is not published for Scala 3, so this module is built for Scala 2.13 only.
lazy val spark = project.dependsOn(core).settings(
  name := "tableparser-spark",
  scalacOptions += silence170Deprecations,
  crossScalaVersions := Seq(scala213),
  libraryDependencies ++= Seq(
    "org.apache.spark" %% "spark-sql" % sparkVersion % "provided",
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val zio = project.dependsOn(core).settings(
  name := "tableparser-zio",
  Test / scalacOptions += silence170Deprecations,
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zioVersion,
    // NOTE: needed in compile scope only by EvaluateZIO, which is deprecated: make it Test only in 2.0.
    "dev.zio" %% "zio-test-junit" % zioVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "dev.zio" %% "zio-test" % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
    "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// The integration tests in core/src/it and cats/src/it run against full-size data files (69 MB).
// They're not aggregated by root, so neither `sbt +test` nor CI runs them: use `sbt integration/test`.
lazy val integration = (project in file("integration"))
        .dependsOn(core % "compile->compile;test->test", cats % "compile->compile;test->test")
        .settings(
          name := "tableparser-integration",
          publish / skip := true,
          Test / parallelExecution := false,
          Test / scalacOptions += silence170Deprecations,
          Test / unmanagedSourceDirectories ++= Seq(
              (LocalRootProject / baseDirectory).value / "core/src/it/scala"
            , (LocalRootProject / baseDirectory).value / "cats/src/it/scala"
          ),
          Test / unmanagedResourceDirectories += (LocalRootProject / baseDirectory).value / "core/src/it/resources",
          libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % Test
        )

lazy val root = (project in file("."))
        .aggregate(core, cats, parquet, spark, zio)
        .settings(
          name := "TableParser",
          // Each module is cross-built with its own crossScalaVersions.
          crossScalaVersions := Nil,
          publish / skip := true
        )

Test / parallelExecution := false