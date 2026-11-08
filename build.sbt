ThisBuild / organization := "com.phasmidsoftware"
ThisBuild / version := "2.5.1"

// 2.x is Scala 3 only (for Scala 2.13, use the 1.x line, on the 1.x branch).
// The current Scala 3 LTS. Code built with 3.9 can only be used from Scala 3.9 or later.
ThisBuild / scalaVersion := "3.9.0"
// The compiler options that all the Phasmid libraries share.
ThisBuild / scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Werror"
)
// Tests may use a value as a statement (for example, in a matcher), so each project's tests skip -Wnonunit-statement.
lazy val testScalacOptions = Test / scalacOptions ~= (_.filterNot(_ == "-Wnonunit-statement"))

// MiMa checks each published module for binary compatibility with the last release (#120): `sbt mimaReportBinaryIssues`.
// The version scheme is PVP (see publish.sbt): a patch release must pass; a minor release may break compatibility,
// on purpose and with a release note, by listing the breaks in mimaBinaryIssueFilters (or, after the release, by moving previousRelease on).
val previousRelease = "2.5.1"
lazy val mimaSettings = mimaPreviousArtifacts := Set(organization.value %% name.value % previousRelease)
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

// sbt's lint wrongly reports these ThisBuild publish settings as unused
Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)


ThisBuild / resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

// Shared versions
val scalaTestVersion = "3.2.20"
val scalaParserCombinatorsVersion = "2.5.0"
val tsecVersion = "0.6.0"
val catsVersion = "3.7.1"
val zioVersion = "2.1.27"
val logbackVersion = "1.5.38"
val parquetVersion = "1.18.1"
val hadoopVersion = "3.4.3"
val circeVersion = "0.14.17"
val fs2Version = "3.14.0"
val visitorVersion = "1.7.0"
val fs2KafkaVersion = "4.1.3"
val zioKafkaVersion = "3.8.1"
val embeddedKafkaVersion = "4.4.0"

lazy val core = project.settings(
  testScalacOptions,
  mimaSettings,
  name := "tableparser-core",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion,
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    // Visitor's PrioQueue (a binary heap) is used by TopN. Visitor's runtime logback is excluded: users choose their own SLF4J backend.
    "com.phasmidsoftware" %% "visitor" % visitorVersion exclude("ch.qos.logback", "logback-classic"),
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// The cats and zio tests use the examples (Movie, Crime), which are in core's tests.
lazy val cats = project.dependsOn(core % "compile->compile;test->test").settings(
  testScalacOptions,
  mimaSettings,
  name := "tableparser-cats",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % catsVersion,
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// Encryption of tables (e.g. of the rows of a CSV file), using tsec, with cats-effect IO.
lazy val crypto = project.dependsOn(core % "compile->compile;test->test", cats % "compile->compile;test->test").settings(
  testScalacOptions,
  mimaSettings,
  name := "tableparser-crypto",
  libraryDependencies ++= Seq(
    "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion,
    "org.typelevel" %% "cats-effect" % catsVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// Reading and writing tables as JSON, using circe.
lazy val json = project.dependsOn(core % "compile->compile;test->test").settings(
  testScalacOptions,
  mimaSettings,
  name := "tableparser-json",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val parquet = project.dependsOn(core).settings(
  testScalacOptions,
  mimaSettings,
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
  testScalacOptions,
  mimaSettings,
  name := "tableparser-zio",
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-streams" % zioVersion,
    "dev.zio" %% "zio-test-junit" % zioVersion % Test,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "dev.zio" %% "zio-test" % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
    "dev.zio" %% "zio-test-magnolia" % zioVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// Sending tables to Kafka, with fs2-kafka (cats-effect IO and fs2 streams).
// The tests use the Kafka client's MockProducer, so they need no broker.
lazy val kafkaCats = (project in file("kafka-cats")).dependsOn(core % "compile->compile;test->test", cats).settings(
  testScalacOptions,
  mimaSettings,
  name := "tableparser-kafka-cats",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "fs2-kafka" % fs2KafkaVersion,
    // circe is only for the test of JSON messages (the README's example): the module itself has no JSON dependency.
    "io.circe" %% "circe-core" % circeVersion % Test,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// Sending tables to Kafka, with zio-kafka (ZIO and ZStream).
// The tests use the Kafka client's MockProducer, so they need no broker.
lazy val kafkaZio = (project in file("kafka-zio")).dependsOn(core % "compile->compile;test->test", zio % "compile->compile;test->test").settings(
  testScalacOptions,
  mimaSettings,
  name := "tableparser-kafka-zio",
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio-kafka" % zioKafkaVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

// The integration tests in core/src/it and cats/src/it run against full-size data files (69 MB);
// those in kafka-cats/src/it and kafka-zio/src/it, against a Kafka broker which they start.
// They're not aggregated by root, so neither `sbt test` nor CI runs them: use `sbt integration/test`.
lazy val integration = (project in file("integration"))
        .dependsOn(core % "compile->compile;test->test", cats % "compile->compile;test->test", crypto % "compile->compile;test->test", kafkaCats % "compile->compile;test->test", kafkaZio % "compile->compile;test->test")
        .settings(
          testScalacOptions,
          name := "tableparser-integration",
          publish / skip := true,
          mimaFailOnNoPrevious := false, // not published, so nothing to compare
          Test / parallelExecution := false,
                  Test / unmanagedSourceDirectories ++= Seq(
              (LocalRootProject / baseDirectory).value / "core/src/it/scala"
            , (LocalRootProject / baseDirectory).value / "cats/src/it/scala"
            , (LocalRootProject / baseDirectory).value / "kafka-cats/src/it/scala"
            , (LocalRootProject / baseDirectory).value / "kafka-zio/src/it/scala"
          ),
          Test / unmanagedResourceDirectories += (LocalRootProject / baseDirectory).value / "core/src/it/resources",
          libraryDependencies ++= Seq(
              "org.scalatest" %% "scalatest" % scalaTestVersion % Test
            // A Kafka broker, in this JVM, for the kafka-cats and kafka-zio integration tests.
            // The broker is built with Scala 2.13: it runs with core's Scala 3 build of scala-logging instead of its own.
            , "io.github.embeddedkafka" %% "embedded-kafka" % embeddedKafkaVersion % Test exclude("com.typesafe.scala-logging", "scala-logging_2.13")
          )
        )

lazy val root = (project in file("."))
        .enablePlugins(ScalaUnidocPlugin, GhpagesPlugin)
        .aggregate(core, cats, crypto, json, kafkaCats, kafkaZio, parquet, zio)
        .settings(
          name := "TableParser",
          publish / skip := true,
          mimaFailOnNoPrevious := false, // not published, so nothing to compare
          // The unified Scaladoc is published to https://rchillyard.github.io/TableParser/latest/api/ by `sbt ghpagesPushSite`
          ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(core, cats, crypto, json, kafkaCats, kafkaZio, parquet, zio),
          ScalaUnidoc / siteSubdirName := "latest/api",
          addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
          // Keep the Scaladoc of older releases, such as 1.7.2/api/, which ghpagesPushSite would otherwise delete.
          ghpagesCleanSite / excludeFilter := new SimpleFileFilter(f => f.isDirectory && f.getName.matches("""\d+\.\d+\.\d+""")),
          git.remoteRepo := "git@github.com:rchillyard/TableParser.git"
        )

Test / parallelExecution := false