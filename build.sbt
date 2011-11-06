name := "knockoff"

organization := "com.tristanhunt"

version := "0.8.0-16"

// I'm not sure how this can be automated for multiple versions like it
// used to be. Knockoff should be buildable from version 2.7.5
scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)

publishTo := Some("scala-tools.org releases" at 
                  "http://nexus.scala-tools.org/content/repositories/releases/")

credentials +=  Credentials(Path.userHome / ".ivy2" / "credentials_scala-tools")