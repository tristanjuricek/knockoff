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

    import com.tristanhunt.knockoff.Imports._
    import java.lang.Math.min

    val blocks = knockoff( markdownString )
    
    val detuned = blocks.foreach{ block => block match {
     
        case header : Header => Header( header.nads, min( header.level + 1, 6 ) )
     
        case _ => block
    }
    
### Another Example: Tweak the last element of any paragraph.



## The Object Model ##

In HTML, you've got Nodes, and everything inherits from this.

In KnockOff, you've got Nads, and spanning elements are groups of these
things. But in any markdown document, there's a nice distinction between
paragraphs and the things in those paragraphs, so I added a block, which
owns these spanning elements.

In another sense, the KnockOff hierarchy is compositional:

A `Block` has many `Spans` which has many `Nads`.

That's abound the long and the short of the object model.