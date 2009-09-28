Coding With Knockoff
====================



### Getting the knockoff dependency

I provide public access to my web server to allow you to quickly use knockoff
without having to setup your projects, etc, provided you have something like sbt,
maven, or ivy configured. The project can be found at:

    http://tristanhunt.com:8081/content/groups/public

Including in sbt is like:

    val knockoff = "com.tristanhunt" % "knockoff" %% "0.5"

(The group name is `com.tristanhunt` and the project ID is `knockoff`.)


### Building knockoff source

This project uses another project, called Literate, to build itself. This is already
included as a plugin, but you have to alter your call to include the macro
processing step. From the base directory, you call:

    sbt update literate package
