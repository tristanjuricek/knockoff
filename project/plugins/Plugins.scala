import sbt._

class Plugins( info : ProjectInfo ) extends PluginDefinition( info ) {
    val tristanhuntNexus = "tristanhunt Nexus" at
        "http://tristanhunt.com:8081/content/groups/public"
    val literable_plugin = "com.tristanhunt" % "knockoff-literable_plugin" % "0.1.0-2"
}