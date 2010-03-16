# Getting Started #



## Using Knockoff With [SBT][] ##

Right now knockoff is easiest to use via [sbt][]. The dependency declaration that
resolves the scala version looks like this:

    val knockoff = "com.tristanhunt" % "knockoff" %% "0.7.0-10"

If you have a problem with the scala version, you can specify it directly as part
of the project name:

    val knockoff = "com.tristanhunt" % "knockoff_2.7.7" % "0.7.0-10"

The repository specification is:

    val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"

A very simple example of the project definition file that goes in a place like
`project/build/MyProject.scala`:

    import sbt._
    
    class MyProject( info : ProjectInfo ) extends DefaultProject( info ) {
      val knockoff  = "com.tristanhunt" % "knockoff_2.7.7" % "0.7.0-10"
      val nexus =
        "tristanhunt nexus" at "http://tristanhunt.com:8081/content/groups/public"
    }



## Using Knockoff With Maven ##

An example dependency definitions includes the scala version as part of the
`artifactId`.

    <dependency>
      <groupId>com.tristanhunt</groupId>
      <artifactId>knockoff_2.7.7</artifactId>
      <version>0.7.0-10</version>
    </dependency>

You'll likely need to grab things off of my nexus host, which can be done with this
repository definition:

    <repository>
      <id>tristanhunt.com</id>
      <name>Tristan's Repo</name>
      <url>http://tristanhunt.com:8081/content/groups/public/</url>
    </repository>



## Using Knockoff outside the Simple Build Tool ##

Definitely possible, it will require a downloaded [scala][] version, after which you
include the `lib/scala-library.jar` file in your classpath along with the [file][1] at

	  http://tristanhunt.com:8081/content/groups/public/com/tristanhunt/
		  knockoff_[SCALA_VERSION]/
		  [KNOCKOFF_VERSION]/
		  knockoff_[SCALA_VERSION]-[KNOCKOFF_VERSION].jar

Yes, it's a long URL. So here's a [link][1] to one of the latest releases.


## Building knockoff source ##

It uses [sbt][]:

    sbt update package

It also uses [literable][], which is what generates both the source code and
documentation. Add the `lit` sub command.

    sbt update lit package


[1]: http://tristanhunt.com:8081/content/groups/public/com/tristanhunt/knockoff_2.7.7/0.7.0-10/knockoff_2.7.7-0.7.0-10.jar
[literable]: http://tristanhunt.com/projects/literable
[sbt]: http://code.google.com/p/simple-build-tool/
[scala]: http://www.scala-lang.org
