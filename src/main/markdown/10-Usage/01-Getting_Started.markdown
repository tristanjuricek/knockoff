Getting Started
===============

Right now knockoff is easiest to use via [sbt][].


### Building with SBT

Add my public repository:

	val tristanhunt =
		"tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"

Include the direct dependency:

    val knockoff = "com.tristanhunt" % "knockoff" %% "0.5.0-1"


### Running outside of SBT

Definitely possible, it will require a downloaded [scala][] version, after which you
include the `lib/scala-library.jar` file in your classpath along with the [file][1] at

	http://tristanhunt.com:8081/content/groups/public/com/tristanhunt/
		knockoff_[SCALA_VERSION]/
		[KNOCKOFF_VERSION]/
		knockoff_[SCALA_VERSION]-[KNOCKOFF_VERSION].jar

Yes, it's a long URL. So here's the [link][1] again.

### Building knockoff source

This project uses another project, called Literate, to build itself. This is already
included as a plugin, but you have to alter your call to include the macro
processing step. From the base directory, you call:

	cd literable_plugin
	sbt update publish-local
	cd ..
    sbt update literate package


[1]: http://tristanhunt.com:8081/content/groups/public/com/tristanhunt/knockoff_2.7.6/0.5.0-1/knockoff_2.7.6-0.5.0-1.jar
[sbt]: http://code.google.com/p/simple-build-tool/
[scala]: http://www.scala-lang.org