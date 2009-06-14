KnockOff - A Parser + Object Model (in Scala)
=============================================

Most of the Markdown-based parsers emulate the Perl-script, where they
generate some HTML. And then you're left to manipulate the rendered HTML
fragment. Which isn't horrible, but could be better. KnockOff enables you to
take your Markdown-based documents and manipulate the output in subtle little
ways. It does this by:

1. Representing the markdown document in an object model, which can then
   be rebuilt.
2. Creating a fairly simple HTML rendering logic, that can also be adjusted.

### Example: "Turning Down" the Header Level

I found this handy: keep the title of your website at h1 by taking any
header in your markdown source, and detune it.

This is an example of tweaking the object model.

    import com.tristanhunt.knockoff._
    import com.tristanhunt.knockoff.Imports._
    import java.lang.Math.min

    val blocks = knockoff( markdownString ) match {
        case KnockOff.Parsed( blocks ) => blocks
        case KnockOff.Failed( message ) => message
    }
    
    val detuned = blocks.foreach{ block => block match {
        case header : Header => Header( header.nads, min( header.level + 1, 6 ) )
        case _ => block
    }
    
    val html = detuned.toXML


### Another Example: Make the first header element have the "title" class.

Here, we'll take the first header element that is rendered, and override the
`class` attribute to be `title` in the final HTML.

This is an example of adjusting the rendering. Note that I might change this
around a bunch soon-ish.

    import com.tristanhunt.knockoff._
    import com.tristanhunt.knockoff.Imports._
    
    BlockConverter.current = new DefaultBlockConverter {

        var isAdded = false

        override def toHeaderXML( header : Header ) : Node = {
            
            isAdded match {
                
                false => super.toHeaderXML( header ) match {
                    case e : Elem => Elem(
                        e.prefix,
                        e.label,
                        new UnprefixedAttribute( "class", "title", e.attributes ),
                        e.scope,
                        e.child
                    )
                }
                
                true => super.toHeaderXML( header )
            }
        }
    }
    
    val xhtml = knockoff( markdownString ).get

A few thoughts:

* I'm not 100% sold on the use of the object member or constructor method,
  even. I know there's a better way.
* We'll have to see how much HTML adjustment people want to do, I find the
  object model adjustments much more useful in practice.


## The Object Model ##

In HTML, you've got Nodes, and everything inherits from this.

In KnockOff, you've got Nads, and spanning elements are groups of these
things. But in any markdown document, there's a nice distinction between
paragraphs and the things in those paragraphs, so I added a block, which
owns these spanning elements.

In another sense, the KnockOff hierarchy is compositional:

A `Block` has many `Spans` which has many `Nads`.

That's abound the long and the short of the object model.