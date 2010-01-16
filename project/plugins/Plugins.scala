import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
    
    val nexus = "tristanhunt nexus" at
      "http://tristanhunt.com:8081/content/groups/public"
    
    override def managedStyle = ManagedStyle.Maven
    
    val literable_plugin_base =
      "com.tristanhunt" % "literable-plugin_2.7.7" % "0.5.11-22"
}