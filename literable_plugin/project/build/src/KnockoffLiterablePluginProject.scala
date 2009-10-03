import sbt._

/**
 * This is the plugin I'll for the real literable project that will produce the project
 * deployment for the tristanhunt.com website.
 */
class KnockoffLiterablePluginProject(info: ProjectInfo) extends PluginProject(info) {

    Credentials(Path.userHome / ".ivy2" / ".credentials", log)

    val nexus = "tristanhunt nexus" at "http://tristanhunt.com:8081/content/groups/public"

    val knockoff    = "com.tristanhunt" % "knockoff" % "0.4.1"
    val site_step   = "com.tristanhunt" % "site_step" % "0.4.0"
    val literable   = "com.tristanhunt" % "literable" % "0.4.5"
    val sbt_plugin  = "com.tristanhunt" % "literable_sbt_plugin" % "0.4.4"
    
    override def managedStyle = ManagedStyle.Maven
    val publishTo = "tristanhunt releases" at "http://tristanhunt.com:8081/content/repositories/releases/"
}