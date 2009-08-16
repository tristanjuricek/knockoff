import sbt._

class KnockoffProject( info : ProjectInfo ) extends DefaultProject( info ) {

    Credentials(Path.userHome / ".ivy2" / ".credentials", log)

    val nexus = "tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"

    val ScalaTest = "org.scala-tools.testing" % "scalatest" % "0.9.5"
    
    override def managedStyle = ManagedStyle.Maven
    val publishTo = "tristanhunt releases" at "http://tristanhunt.com:8081/content/repositories/releases/"
}