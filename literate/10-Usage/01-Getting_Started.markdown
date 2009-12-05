Getting Started
===============

To run the following scala code:

    import com.tristanhunt.knockoff.DefaultDiscounter.knockoff
    
    knockoff( markdownTextString ).toXML
    
You need to have the knockoff build.


## Using Knockoff with the Simple Build Tool ##

Right now knockoff is easiest to use via [sbt][].

Include the direct dependency:

    val knockoff = "com.tristanhunt" % "knockoff" %% "0.6.1-SNAPSHOT"

You can grab it off of my repository:

    val tristanhunt =
      "tristanhunt" at "http://tristanhunt.com:8081/content/groups/public/"


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


[1]: http://tristanhunt.com:8081/content/groups/public/com/tristanhunt/knockoff_2.7.7/0.6.1-SNAPSHOT/knockoff_2.7.7-0.6.1-SNAPSHOT.jar
[literable]: http://tristanhunt.com/projects/literable
[sbt]: http://code.google.com/p/simple-build-tool/
[scala]: http://www.scala-lang.org