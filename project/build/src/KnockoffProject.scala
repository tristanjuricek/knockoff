import sbt._

class KnockoffProject( info : ProjectInfo ) extends DefaultProject( info )
  with KnockoffLiterableProject with posterous.Publish {
  
  override def compileOptions = {
    List( MaxCompileErrors( 10 ), CompileOption("-unchecked") ) :::
    super.compileOptions.toList
  }
  
  override def mainClass = Some("com.tristanhunt.knockoff.DefaultDiscounter")
  
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  
  val tristanhunt          = "tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"
  val tristanhuntSnapshots = "tristanhunt Snapshots" at "http://tristanhunt.com:8081/content/groups/public-snapshots/"
  
  // TODO -> For 2.8.0.* series, figure out how to change test dependency.
  // val scala_test = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC7-SNAPSHOT" % "test->default"
  val scala_test = "org.scalatest" % "scalatest" % "1.0.1" % "test->default"
  val jtidy = "jtidy" % "jtidy" % "r938" % "test->default"
  val snuggletex = "uk.ac.ed.ph.snuggletex" % "snuggletex-core" % "1.2.2"
    
  override def managedStyle = ManagedStyle.Maven
  val publishTo = "tristanhunt releases" at
    "http://tristanhunt.com:8081/content/repositories/releases/"
}