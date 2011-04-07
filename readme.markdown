# [Knockoff - Markdown in Scala](http://tristanhunt.com/projects/knockoff) #

This is a simple Markdown to object model to XHTML system.

    import com.tristanhunt.knockoff.DefaultDiscounter._
    
    toXHTML(knockoff("""# My Markdown Content """))

You can use the blocks returned from the `knockoff` method to do useful things, like fetch the header:

    val blocks = knockoff("""# My markdown""")
    blocks.find( _.isInstanceOf[Header] ).map( toText ).getOrElse( "No header" )

## Using the latest version

In sbt:

    val scalatools = "scalatools" at "http://scala-tools.org/repo-releases"
    
    val knockoff  = "com.tristanhunt" %% "knockoff" % "0.8.0-16"

In maven:

    <dependency>
      <groupId>com.tristanhunt</groupId>
      <artifactId>knockoff_2.8.1</artifactId>
      <version>0.8.0-16</version>
    </dependency>

## More information

See the [home page](http://tristanhunt.com/projects/knockoff) for more information: [http://tristanhunt.com/projects/knockoff](). This page is somewhat out-of-date, because I'm going to be altering my approach to the site's generation for a little bit.

License is BSD. Patches are welcome, if the patch is clean, I'll probably accept it.
