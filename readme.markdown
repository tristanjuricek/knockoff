# [Knockoff - Markdown in Scala](http://tristanhunt.com/projects/knockoff) #

This is a simple Markdown to object model to XHTML system.

    import com.tristanhunt.knockoff.DefaultDiscounter._
    
    toXHTML(knockoff("""# My Markdown Content """))

You can use the blocks returned from the `knockoff` method to do useful things, like fetch the header:

    val blocks = knockoff("""# My markdown""")
    blocks.find( _.isInstanceOf[Header] ).map( toText ).getOrElse( "No header" )

## Using the latest version

The short story, in an sbt project/Build.scala file:

      lazy val root = Project("root", file(".")) dependsOn(knockoffProject)
      lazy val knockoffProject = RootProject(uri(
          "git://github.com/tristanjuricek/knockoff.git"))

The longer version can be read on this [nice dev daily overview](http://www.devdaily.com/scala/using-github-projects-scala-library-dependencies-sbt-sbteclipse).

## More information

See the [home page](http://tristanhunt.com/projects/knockoff) for more information: [http://tristanhunt.com/projects/knockoff]().

License is BSD. Patches are welcome, if the patch is clean, I'll probably accept it.
