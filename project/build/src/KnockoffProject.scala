import sbt._

class KnockoffProject( info : ProjectInfo ) extends ParentProject( info ) {
  
  val tristanhunt          = "tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"
  val tristanhuntSnapshots = "tristanhunt Snapshots" at "http://tristanhunt.com:8081/content/groups/public-snapshots/"
  val scalatools           = "scalatools" at "http://scala-tools.org/repo-releases"
  
  lazy val knockoff = project("knockoff", "knockoff", new KnockoffProject(_) )
  lazy val knockoffExtras = project("knockoff-extras", "knockoff-extras", new KnockoffExtrasProject(_), knockoff)
  
  // lazy val knockoffGenerator = project("knockoff-generator", "generator",
  //                                      new GeneratorProject(_), knockoff )
  
  class KnockoffProject( info : ProjectInfo ) extends DefaultProject(info)
  with KnockoffLiterableProject with posterous.Publish {
    override def mainClass = Some("com.tristanhunt.knockoff.DefaultDiscounter")
    Credentials(Path.userHome / ".ivy2" / ".credentials", log)
    
    val scala_test = "org.scalatest" % "scalatest" % "1.4-SNAPSHOT" % "test->default"  // >= 

    // val jtidy = "jtidy" % "jtidy" % "r938" % "test->default"
    
    override def managedStyle = ManagedStyle.Maven
    val publishTo = "tristanhunt releases" at
      "http://tristanhunt.com:8081/content/repositories/releases/"    
  }

  class KnockoffExtrasProject( info : ProjectInfo ) extends DefaultProject(info)
  with KnockoffLiterableProject with posterous.Publish {
    
    Credentials(Path.userHome / ".ivy2" / ".credentials", log)

    // val snuggletex = "uk.ac.ed.ph.snuggletex" % "snuggletex-core" % "1.2.2"    
  }

  // Requires scala 2.8?
  // class GeneratorProject( info : ProjectInfo ) extends DefaultProject(info) with KnockoffLiterableProject {
  //   val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.7"
  //   override def consoleInit =
  //    """import com.tristanhunt.knockoff.generator.MarkdownGenerator._"""
  // }
}



