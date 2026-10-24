ThisBuild / organization := "com.phasmidsoftware"
ThisBuild / version := "2.0.0-SNAPSHOT"

// 2.x is Scala 3 only (for Scala 2.13, use the 1.x line, on the 1.x branch).
// Build with the oldest supported LTS so that the artifacts are usable from any later Scala 3 version.
// CircleCI also runs the tests on the current LTS (3.9).
ThisBuild / scalaVersion := "3.3.8"
ThisBuild / scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-feature")
ThisBuild / scalacOptions ++= Seq("-java-output-version", "21")
ThisBuild / javacOptions ++= Seq("-source", "21", "-target", "21")

// Enforce Java 21 minimum (2.x targets Java 21; for Java 17, use the 1.x line)
ThisBuild / initialize := {
  val _ = initialize.value
  val required = "21"
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
val tsecVersion = "0.6.0"
val catsVersion = "3.7.1"
val zioVersion = "2.1.27"
val logbackVersion = "1.5.38"
val slf4jVersion = "2.0.20"
val parquetVersion = "1.18.1"
val hadoopVersion = "3.4.3"
val circeVersion = "0.14.17"

lazy val core = project.settings(
  name := "tableparser-core",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// The cats and zio tests use the examples (Movie, Crime), which are in core's tests.
lazy val cats = project.dependsOn(core % "compile->compile;test->test").settings(
  name := "tableparser-cats",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % catsVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// Encryption of tables (e.g. of the rows of a CSV file), using tsec, with cats-effect IO.
lazy val crypto = project.dependsOn(core % "compile->compile;test->test", cats % "compile->compile;test->test").settings(
  name := "tableparser-crypto",
  libraryDependencies ++= Seq(
    "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion,
    "org.typelevel" %% "cats-effect" % catsVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// Reading and writing tables as JSON, using circe.
lazy val json = project.dependsOn(core % "compile->compile;test->test").settings(
  name := "tableparser-json",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
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

lazy val zio = project.dependsOn(core % "compile->compile;test->test").settings(
  name := "tableparser-zio",
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-test-junit" % zioVersion % Test,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "dev.zio" %% "zio-test" % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
    "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    "org.slf4j" % "slf4j-simple" % slf4jVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// The integration tests in core/src/it and cats/src/it run against full-size data files (69 MB).
// They're not aggregated by root, so neither `sbt test` nor CI runs them: use `sbt integration/test`.
lazy val integration = (project in file("integration"))
        .dependsOn(core % "compile->compile;test->test", cats % "compile->compile;test->test", crypto % "compile->compile;test->test")
        .settings(
          name := "tableparser-integration",
          publish / skip := true,
          Test / parallelExecution := false,
                  Test / unmanagedSourceDirectories ++= Seq(
              (LocalRootProject / baseDirectory).value / "core/src/it/scala"
            , (LocalRootProject / baseDirectory).value / "cats/src/it/scala"
          ),
          Test / unmanagedResourceDirectories += (LocalRootProject / baseDirectory).value / "core/src/it/resources",
          libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % Test
        )

lazy val root = (project in file("."))
        .aggregate(core, cats, crypto, json, parquet, zio)
        .settings(
          name := "TableParser",
          publish / skip := true
        )

Test / parallelExecution := false