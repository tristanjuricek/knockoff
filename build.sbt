lazy val commonSettings = Seq(
    version:="0.8.3",
    scalaVersion:="2.10.4",
    crossScalaVersions := Seq("2.10.4", "2.11.4"),
    organization := "com.tristanhunt",
    licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
    homepage := Some(url("http://tristanjuricek.com/projects/knockoff")),
    scmInfo := Some(ScmInfo(
      url("https://github.com/tristanjuricek/knockoff.git"),
      "git:git@github.com:tristanjuricek/knockoff.git")),
    developers := List(Developer("tjuricek", "Tristan Juricek", "", url("http://tristanjuricek.com"))),
    useGpg := true,
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false }
  )

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name:="knockoff",
    description := "A simple Markdown to object model to XHTML system.",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "junit" % "junit" % "4.11" % "test",
      "net.sf.jtidy" % "jtidy" % "r938" % "test"
    ),
    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value ++ Seq(
            "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
          )
        case _ =>
          libraryDependencies.value 
      }
    }
  )
