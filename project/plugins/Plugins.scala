import sbt._

class Plugins( info : ProjectInfo ) extends PluginDefinition( info ) {
    val tristanhuntNexus = "tristanhunt Nexus" at
        "http://tristanhunt.com:8081/content/groups/public"
    val literable_plugin = "com.tristanhunt" % "tristanhunt_plugin" % "0.4.5"
}