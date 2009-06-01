import sbt._

class KnockoffProject( info : ProjectInfo ) extends DefaultProject( info ) {
    
    val emarsysNexus = "emarsys Nexus" at "http://nexus.emarsys.com/content/groups/public"

    val ScalaTest = "org.scala-tools.testing" % "scalatest" % "0.9.5"
}