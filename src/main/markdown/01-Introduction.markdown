Introduction to Knockoff
========================

Knockoff is a Markdown parser with a twist. Most Markdown systems just convert
Markdown source to HTML. Knockoff converts from Markdown source to an
in-memory model to HTML.


## New Ideas ##

### Make Iterating The Tree Easier

The XML library has expressions like:

    head \ "child"

To grab all child elements. Could we create a virtual sequence similar to
`NodeSeq` that does the same sort of thing? This would then be "BlockSeq"
which also contains "SpanSeq". You could then grab specific types:

    ( seq \ Header ).firstOption match {
        
    }
    
    ( seq \ CodeBlock ).foreach { codeBlock =>
        // TODO: lines...
    }

Other idea would be to include some kind of search method

    // ACH -> this is not quite correct
    ( seq \ Paragraph ?
        _.markdown.getLines( line => line.startsWith( "Title: " ) )
    )


### Converting the XML

The final coup de grace would be a XML convenience method to convert one Elem
to another Elem. I'm not sure how to implement this however.


### Solidify The Markdown Versus HTML Representations Simpler

One advantage of HTML is that the object model could just contain the XML
representation rather than requiring a second conversion step.

    scala> header.markdown
    java.lang.String = # My Header #
    
    scala> header.xml
    scala.xml.Elem = <h1>My Header</h1>

The advantage here: the user no longer has to override any "global objects" in
the system.


### Allow The Position Of The Model To Come Through

Say we have the document with line numbers:

    1: Header 1
    2: ========
    3:
    4: Paragraph one
    5:
    6:     Code block 1
    7:
    8: Paragraph two

It would be _very_ handy to have the line numbers. This allows us to backwards
patch into the original markdown document.

    scala> blockSeq(2).position.startLine
    Int = 6
    
    scala> blockSeq(0).position.endLine
    Int = 2

Note that this is only significant for `Block` elements.