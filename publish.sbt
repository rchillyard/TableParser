// Optional but useful
ThisBuild / description := "Scala library to process tabular data, for example, CSV files"
ThisBuild / homepage := Some(url("https://github.com/rchillyard/TableParser"))
ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

// Publishing configuration
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true

// Credentials
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")

// Required POM metadata for Maven Central
ThisBuild / licenses := List("Apache-2.0" -> new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rchillyard/TableParser"),
    "scm:git@github.com:rchillyard/TableParser.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "rchillyard",
    name = "Robin Hillyard",
    email = "rchillyard@phasmidsoftware.com",
    url = url("https://phasmidsoftware.com")
  )
)

// Optional

// Publishing settings
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true)
ThisBuild / publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)

// Optional: Skip publishing of docs and sources for snapshot versions
ThisBuild / publishArtifact := true

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
