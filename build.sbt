ThisBuild / organization := "com.phasmidsoftware"

name := "TableParser"

ThisBuild / version := "1.1.4-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val core = project in file("core")

lazy val cats = project in file("cats")

lazy val spark = project in file("spark")

lazy val root = (project in file(".")).aggregate(core, cats, spark)

Test / parallelExecution := false

//javaOptions ++= Seq("-Xms512M", "-Xmx2048M", "-XX:+CMSClassUnloadingEnabled")

scalacOptions += "-deprecation"

// NOTE: if you reinstate these directories, you will need to manage the large crimes file (see code).
//Test / unmanagedSourceDirectories += baseDirectory.value / "src/it/scala"
//Test / unmanagedResourceDirectories += baseDirectory.value / "src/it/resources"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

