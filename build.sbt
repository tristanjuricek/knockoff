name := "knockoff"

organization := "com.tristanhunt"

version := "0.8.1"

scalaVersion := "2.9.2"

crossScalaVersions := Seq(
  "2.9.2", "2.9.1-1", "2.9.1", "2.9.0-1", "2.9.0", "2.8.2"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

// Publishing setup to Sonatype's OSS hosting.
//
// We generally do not publish anything but releases for this project.

publishMavenStyle := true

// Do not publish test artifacts
publishArtifact in Test := false

pomExtra := (
  <url>http://tristanjuricek.com/knockoff</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>https://github.com/tristanjuricek/knockoff/blob/master/license.txt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git://github.com/tristanjuricek/knockoff.git</url>
    <connection>scm:git:git@github.com:tristanjuricek/knockoff.git</connection>
  </scm>
  <developers>
    <developer>
      <id>tristanjuricek</id>
      <name>Tristan Juricek</name>
      <url>http://tristanjuricek.com</url>
    </developer>
  </developers>)

publishTo := Some("knockoff Sonatype releases" at
                  "https://oss.sonatype.org/service/local/staging/deploy/maven2")

credentials += Credentials(Path.userHome / ".sbt" / "sonatype.sbt")
