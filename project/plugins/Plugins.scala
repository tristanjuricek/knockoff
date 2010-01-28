import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
    
    val nexus = "tristanhunt nexus" at
      "http://tristanhunt.com:8081/content/groups/public"
    
    override def managedStyle = ManagedStyle.Maven
    
    val literable_plugin_base =
      "com.tristanhunt" % "literable-plugin_2.7.7" % "0.5.12-23"
      
  val newReleaseToolsRepository = "Scala Tools Repository" at
    "http://nexus.scala-tools.org/content/repositories/snapshots/" 
  val scalatest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" % "test"
}