ThisBuild / organization := "com.phasmidsoftware"

name := "TableParser"

ThisBuild / version := "1.2.4"

scalaVersion := "2.13.16"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation")
scalacOptions ++= Seq("-java-output-version", "17")
javacOptions ++= Seq("-source", "17", "-target", "17")

// Enforce Java 17 minimum
initialize := {
  val _ = initialize.value
  val required = "17"
  val current = sys.props("java.specification.version")
  assert(
    current.toDouble >= required.toDouble,
    s"Java $required or higher required. Current version: $current"
  )
}

lazy val core = project

lazy val cats = project.dependsOn(core)

lazy val zio = project.dependsOn(core)

lazy val spark = project.dependsOn(core)

lazy val root = (project in file(".")).aggregate(core, cats, zio, spark)

Test / parallelExecution := false

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

