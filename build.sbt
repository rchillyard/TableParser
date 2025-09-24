ThisBuild / organization := "com.phasmidsoftware"

name := "TableParser"

ThisBuild / version := "1.2.3"

scalaVersion := "2.13.16"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")

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

