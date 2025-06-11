ThisBuild / organization := "com.phasmidsoftware"

name := "TableParser"

ThisBuild / version := "1.2.1-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val core = project

lazy val cats = project.dependsOn(core)

lazy val spark = project.dependsOn(core)

lazy val root = (project in file(".")).aggregate(core, cats, spark)

Test / parallelExecution := false

//javaOptions ++= Seq("-Xms512M", "-Xmx2048M", "-XX:+CMSClassUnloadingEnabled")

scalacOptions += "-deprecation"

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

