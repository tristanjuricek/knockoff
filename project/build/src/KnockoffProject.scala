import sbt._

class KnockoffProject( info : ProjectInfo ) extends ParentProject( info ) {
  
  val tristanhunt          = "tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"
  val tristanhuntSnapshots = "tristanhunt Snapshots" at "http://tristanhunt.com:8081/content/groups/public-snapshots/"
  val scalatools           = "scalatools" at "http://scala-tools.org/repo-releases"
  
  lazy val knockoff = project("knockoff", "knockoff", new KnockoffProject(_) )
  
  // lazy val knockoffGenerator = project("knockoff-generator", "generator",
  //                                      new GeneratorProject(_), knockoff )
  
  class KnockoffProject( info : ProjectInfo ) extends DefaultProject(info)
  with KnockoffLiterableProject with posterous.Publish {
    override def mainClass = Some("com.tristanhunt.knockoff.DefaultDiscounter")
    Credentials(Path.userHome / ".ivy2" / ".credentials", log)
    
    // TODO -> For 2.8.0.* series, figure out how to change test dependency.
    // val scala_test = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC7-SNAPSHOT" % "test->default"
    //val scala_test = "org.scalatest" % "scalatest" % "1.2" % "test->default"  // >= 2.8.0
    val scala_test = "org.scalatest" % "scalatest" % "1.1" % "test->default" // < 2.8.0
    val jtidy = "jtidy" % "jtidy" % "r938" % "test->default"
    val snuggletex = "uk.ac.ed.ph.snuggletex" % "snuggletex-core" % "1.2.2"
    
    override def managedStyle = ManagedStyle.Maven
    val publishTo = "tristanhunt releases" at
      "http://tristanhunt.com:8081/content/repositories/releases/"    
  }

  // Requires scala 2.8?
  // class GeneratorProject( info : ProjectInfo ) extends DefaultProject(info) with KnockoffLiterableProject {
  //   val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.7"
  //   override def consoleInit =
  //    """import com.tristanhunt.knockoff.generator.MarkdownGenerator._"""
  // }
}



