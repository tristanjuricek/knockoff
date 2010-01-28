# LaTeX Conversion #

I'm going to experiment with a `toLatex` method on the `Wholesaler`. Additionally,
I'd like to allow some [LaTeX][] _pass-through_ capability.

Two major goals:

1. Allow a nice print format for markdown documents.
2. Allow "mostly markdown" notation, which might be output to both MathML or TeX.

The second part is to avoid having to come up with Yet Another DSL inside Markdown
when TeX is pretty well established.



## LaTeX Output ##


The conversion is rather simple, as we're basically going from one markup format to
another. The intention, however, is to leave the markdown as fragments to be included
in another kind of template.

    import com.tristanhunt.knockoff.DefaultWholesaler._
    val latex_content = knockoff( markdownString ).map( toLatex )
    
    // Maybe you have a {{latex-content}} placeholder in a string template.
    articleTemplate.replace( "{{latex-content}}", latex_content )

Based on this principle, I've also provided a template system that will replace
`{{file_name.markdown}}` with any file that the template system will find.

    val process = processLatexTemplate( dir )
    process( template )

(Or, you can just call `processLatexTemplate( dir )( template )`.)



## LaTeX in The Markdown ##


A series of methods associates each core Knockoff object instances with a `toLatex`
method. Generating the latex

    import Wholesaler._
    blocks.map( toLatex )
    
The main `toLatex` method that takes a single `Block` instance delegates based on
the exact type. Additionally, each `Block` may have `Span` elements which are pretty
much structured in the same way. A top-level `toLatex` methods for a single spanning
element delegates to specific methods that do the real work.

    -> In com/tristanhunt/knockoff/LatexWriter.scala
    package com.tristanhunt.knockoff.extra
    
    trait LatexWriter {
      
      def toLatex( block : Block ) : String = block match {
        case p : Paragraph => toLatex( p )
        case _ => block.toString
      }
      
      /**
        Paragraphs need to have an extra newline to make sure the LaTeX system sees
        a new block.
      */
      def toLatex( p : Paragraph ) : String =
        p.children.map( toLatex ).mkString("\n") + "\n\n"
      
      def toLatex( span : Span ) : String = span match {
        case text : Text                => toLatex( text )
        case html : HTMLSpan            => toLatex( html )
        case code : CodeSpan            => toLatex( code )
        case strong : Strong            => toLatex( strong )
        case em : Emphasis              => toLatex( em )
        case img : ImageLink            => toLatex( img )
        case link : Link                => toLatex( link )
        case _ => span.toString
      }
      
      /**
        TODO: Need to escape out TeX instructions.
      */
      def toLatex( text : Text ) : String = {
        text.unescape( text.content )
      }
      
      /**
        Unfortunately, we won't be able to do much here. The entire thing will be
        inserted as fixed font, which should stand out to the eye, at least.
      */
      def toLatex( html : HTMLSpan ) : String = fixedLatex( html.content )
      
      def toLatex( code : CodeSpan ) : String = fixedLatex( code.content )
      
      def toLatex( str : Strong ) : String =
        """\textbf{""" + str.children.map( toLatex ).mkString("") + "}"
      
      def toLatex( em : Emphasis ) : String =
        """\emph{""" + em.children.map( toLatex ).mkString("") + "}"

      def toLatex( li : Link ) : String = {
        li.children.map( toLatex ).mkString("") + """\footnote{""" +
        li.title.getOrElse("") + " " + li.url + "}"
      }

      def toLatex( linkRef : IndirectLink ) : String =
      
      // See the image conversion for latex documents
      
      def fixedLatex( str : String ) = """\texttt{""" + str + "}"
    }

### Handling Images

**TODO** Investigate how images are added into TeX.

If you can't handle them, include as a footnote?

Hm.

    // The image conversion for latex documents
    def toLatex( img : ImageLink ) : String =
      img.children.map( toLatex ).mkString("") + """\footnote


[latex]: http://www.latex-project.org/ "LaTeX Project Homepage"