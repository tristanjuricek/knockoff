import sbt._

class KnockoffProject( info : ProjectInfo ) extends DefaultProject( info )
  with KnockoffLiterableProject {
  
  override def compileOptions = {
    List( MaxCompileErrors( 10 ), CompileOption("-unchecked") ) :::
    super.compileOptions.toList
  }
  
  override def mainClass = Some("com.tristanhunt.knockoff.DefaultDiscounter")
  
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  val nexus = "tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"

  val scala_test = "org.scalatest" % "scalatest" % "1.0" % "test->default"
  val jtidy = "jtidy" % "jtidy" % "r938" % "test->default"
  
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "tristanhunt releases" at
    "http://tristanhunt.com:8081/content/repositories/releases/"
}