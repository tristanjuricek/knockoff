name := "knockoff"

organization := "com.tristanhunt"

version := "0.8.0-16"

// I'm not sure how this can be automated for multiple versions like it
// used to be. Knockoff should be buildable from version 2.7.5
scalaVersion := "2.9.2"

crossScalaVersions := Seq(
  "2.9.2", "2.9.1-1", "2.9.1",
  "2.9.0-1", "2.9.0", "2.8.2", "2.8.1"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.2" % "test"

publishTo := Some("scala-tools.org releases" at 
                  "http://nexus.scala-tools.org/content/repositories/releases/")

credentials +=  Credentials(Path.userHome / ".ivy2" / "credentials_scala-tools")
