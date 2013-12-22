# [Knockoff - Markdown in Scala](http://tristanhunt.com/projects/knockoff) #

This is a simple Markdown to object model to XHTML system.

    import com.tristanhunt.knockoff.DefaultDiscounter._

    toXHTML(knockoff("""# My Markdown Content """))

You can use the blocks returned from the `knockoff` method to do useful things, like fetch the header:

    val blocks = knockoff("""# My markdown""")
    blocks.find( _.isInstanceOf[Header] ).map( toText ).getOrElse( "No header" )

## More information

See the [home page](http://tristanjuricek.com/knockoff) for more information: [http://tristanjuricek.com/knockoff]().

License is BSD. Patches are welcome, if the patch is clean, I'll probably accept it.
