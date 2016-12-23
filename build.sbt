name:="knockoff"

version:="0.8.3"

scalaVersion:="2.12.1"

crossScalaVersions := Seq("2.10.4", "2.11.4", "2.12.1")

organization := "com.tristanhunt"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "junit" % "junit" % "4.11" % "test",
  "net.sf.jtidy" % "jtidy" % "r938" % "test"
)

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor == 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
      )
    case Some((2, scalaMajor)) if scalaMajor >= 12 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
      )
    case _ =>
      libraryDependencies.value 
  }
}

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://tristanjuricek.com/projects/knockoff</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:tristanjuricek/knockoff.git</url>
    <connection>scm:git:git@github.com:tristanjuricek/knockoff.git</connection>
  </scm>
  <developers>
    <developer>
      <id>tjuricek</id>
      <name>Tristan Juricek</name>
      <url>http://tristanjuricek.com</url>
    </developer>
  </developers>)
