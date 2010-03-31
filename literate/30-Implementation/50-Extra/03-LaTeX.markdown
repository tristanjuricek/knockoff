# LaTeX Writing #

This will write the object model as a LaTeX document _fragment_, intended to be
included in some kind of template system. And, if you're creating TeX, you'll
probably want a few math formulae. Thus, this will also add a couple of block types
that will handle formulae into the system.



# You've Got LaTeX In Your Markdown #

There are two ways to include LaTeX in a markdown document with this extension; as
a span or a block. The span, or _text style_ equation, is wrapped with dollar signs
`$`. The block, or _display style_ equation, is a codeblock with `\begin` and `\end`
LaTeX lines.

You can pretty much add whatever you want in between, these statements are mostly
going to be passed through without a lot of fuss.

    This has a simple formula $a + b$ or $b + d = c$ in a markdown sentence. A more
    well known formula is:
    
        \begin{equation}
        E = mc^2 \label{clever}
        \end{equation}

Extensions for LaTeX in the markdown are simple. The LaTeX-specific markup is passed
through as strings.

    // The LaTeX object model
    case class LatexSpan( latex : String ) extends Span
    
    case class LatexBlock( latex : String, position : Position ) extends Block

These are to be discovered in a post-processing step that can wrap the base
`knockoff` method.

    // The LatexDiscounter
    trait LatexDiscounter extends Discounter {
      
      override def knockoff( source : CharSequence ) : Seq[Block] =
        super.knockoff( source ).map( toLatex )

      def toLatex( block : Block ) : Block = block match {
        case Paragraph( spans, pos ) =>
          Paragraph( spans.flatMap( toLatex ), pos )
        case CodeBlock( text, pos ) =>
          if ( isLatex( text.content ) ) LatexBlock( text.content, pos ) else block
        case Blockquote( children, pos ) =>
          Blockquote( children.map( toLatex ), pos )
        case OrderedList( items ) =>
          OrderedList( items.map{ li => OrderedItem( li.children.map( toLatex ),
                                                     li.position ) } )
        case UnorderedList( items ) =>
          UnorderedList( items.map{ li => UnorderedItem( li.children.map( toLatex ),
                                                         li.position ) } )
        case _ => block
      }
      
      def toLatex( span : Span ) : Seq[Span] = span match {
        case text : Text => splitLatex( text )
        case _ => List(span)
      }
      
      /** Find the next two un-escaped LaTeX characters. Unescape \$ sequences in
          text. */
      private def splitLatex( text : Text ) : Seq[Span] = {
        val idx = text.content.indexOf('$')
        if ( idx > 0 && text.content(idx - 1) != '\\' )
          return splitLatex( text.content, 0, new ListBuffer )
        return List(text)
      }
      
      private def splitLatex( src : String, from : Int, cur : Buffer[Span] )
                            : Seq[Span] = {
        var to = from + 1        
        while ( to != -1 && src( to - 1) == '\\' )
          to = src.indexOf( '$', to + 1 )
        
        if ( to == -1 )
          return ( cur + Text( src.substring( from ) ) )
        
        cur += LatexSpan( src.substring( from, to ) )
        
        val next = src.indexOf( '$', to + 1 )
        if ( next != -1 )
          splitLatex( src, next, cur + Text( src.substring( to, next ) ) )
        else
          cur + Text( src.substring( to ) )
      }
      
      // If first real line begins with \begin and last real line begins with \end,
      // it's latex.
      def isLatex( code : String ) : Boolean = {
        val lines = code.split("\n").filter( ! _.trim.isEmpty ).toList
        return ( 2 <= lines.length &&
                 lines.head.trim.startsWith("\\begin") &&
                 lines.last.trim.startsWith("\\end") )
      }
    }
    


## The `LatexWriter` ##

This enables a slightly different call:

    import discounter._
    toLatex( knockoff( markdownString ) )

Since it's just string building, you can alternatively pass in your own `Writer`.

### Correctness with bold and emphasis

Right now this code outputs both bold and emphasis text into LaTeX. Which is the
easy thing to do and the TeX processor generally ignores things anyway. Might change
in the future.

### Image handling

Knockoff isn't really going to do much but assume that the document is set up to
include things via the `includegraphics` command. It's very likely that the image
file you're using for a website is not what you want to use for print, so here is
where you're better served by using some kind of template system instead of knockoff
directly.

Shameless plug: I'm working on a Velocity plugin that would deal with this sort of
thing exactly.

### HTML Handling with Latex

Right now, any HTML is just not output into the LaTeX document. In the future, I
might add a couple of features to first, convert HTML tables into LaTeX tables -
which first assumes I can parse the HTML nicely. And assuming I can parse that HTML,
the rest of the HTML might just have the text content stripped out and passed on.

    // The LatexWriter
    trait LatexWriter {
      
      def ruleWidth : String = "\\textwidth"
      def ruleHeight : String = ".3pt"
      
      def toLatex( blocks : Seq[Block] ) : String = {
        val sw = new StringWriter
        toLatex( blocks, sw )
        sw.getBuffer.toString
      }
      
      def toLatex( blocks : Seq[Block], writer : Writer ) : Unit =
        toLatex( blocks, 0 )( writer )
      
      def toLatex( blocks : Seq[Block], depth : Int )( implicit writer : Writer )
                 : Unit =
        blocks.foreach { toLatex( _, depth ) }
      
      def toLatex( block : Block, depth : Int )( implicit writer : Writer )
                 : Unit = {
        block match {
          case Paragraph( spans, _ ) => paragraphToLatex( spans )
          case Header( level, spans, _ ) => headerToLatex( level, spans )
          case LinkDefinition( _, _, _, _ ) => {}
          case Blockquote( children, _ ) => blockquoteToLatex( children, depth )
          case CodeBlock( text, _ ) => codeToLatex( text.content )
          case HorizontalRule( _ ) => latexRule
          case OrderedItem( children, _ ) => liToLatex( children, depth )
          case UnorderedItem( children, _ ) => liToLatex( children, depth )
          case OrderedList( items ) => olToLatex( items, depth )
          case UnorderedList( items ) => ulToLatex( items, depth )
          case LatexBlock( latex, _ ) => writer.write( latex )
          case _ => {}
        }
        if ( depth == 0 ) writer.write("\n")
      }
      
      def paragraphToLatex( spans : Seq[Span] )( implicit writer : Writer ) : Unit = {
        spans.foreach( toLatex )
        writer.write("\n")
      }
      
      /** Attempts to map the header as article sections. */
      def headerToLatex( level : Int, spans : Seq[Span] )( implicit writer : Writer )
                       : Unit = {
        level match {
          case 1 => writer.write( "\\section{" )
          case 2 => writer.write( "\\subsection{" )
          case _ => writer.write( "\\subsubsection{" )
        }
        spans.foreach( toLatex )
        writer.write("}\n")
      }
      
      def blockquoteToLatex( children : Seq[Block], depth : Int )
                           ( implicit writer : Writer )
                           : Unit = {
        writer.write( "\\begin{quote}\n" )
        toLatex( children, depth + 1 )
        writer.write( "\\end{quote}\n" )
      }
      
      def codeToLatex( text : String )( implicit writer : Writer ) : Unit = {
        writer.write( "\\begin{verbatim}\n" )
        writer.write( text )
        writer.write( "\\end{verbatim}\n" )
      }
      
      def latexRule( implicit writer : Writer ) : Unit =
        List( "\\rule{", ruleWidth, "}{", ruleHeight, "}\n" )
          .foreach( writer.write )
      
      def olToLatex( items : Seq[OrderedItem], depth : Int )
                   ( implicit writer : Writer )
                   : Unit = {
        writer.write( "\\begin{enumerate}\n" )
        toLatex( items, depth + 1 )
        writer.write( "\\end{enumerate}\n" )
      }
      
      def ulToLatex( items : Seq[UnorderedItem], depth : Int )
                   ( implicit writer : Writer )
                   : Unit = {
        writer.write( "\\begin{itemize}\n" )
        toLatex( items, depth + 1 )
        writer.write( "\\end{itemize}\n" )
      }
      
      def liToLatex( items : Seq[Block], depth : Int )( implicit writer : Writer )
                   : Unit = {
        writer.write( "\\item " )
        toLatex( items, depth )
      }
      
      def toLatex( span : Span )( implicit writer : Writer ) : Unit = span match {
        case Text( content ) => textToLatex( content )
        case HTMLSpan( html ) => htmlSpanToLatex( html )
        case CodeSpan( code ) => codeSpanToLatex( code )
        case Strong( children ) => strongToLatex( children )
        case Emphasis( children ) => emphasisToLatex( children )
        case Link( children, url, title ) => linkToLatex( children, url, title )
        case IndirectLink( children, definition ) =>
          linkToLatex( children, definition.url, definition.title )
        case ImageLink( children, url, title ) => imageLinkToLatex( children, url, title )
        case IndirectImageLink( children, definition ) =>
          imageLinkToLatex( children, definition.url, definition.title )
        case LatexSpan( content ) => writer.write( content )
        case _ => {}
      }
      
      def textToLatex( content : String )( implicit writer : Writer ) : Unit = {
        writer.write( content )
      }
      
      def htmlSpanToLatex( html : String )( implicit writer : Writer ) : Unit = {
        // do nothing for now...
      }
      
      def codeSpanToLatex( code : String )( implicit writer : Writer ) : Unit = {
        writer.write( "\\verb|" )
        writer.write( code.replace("|", "\\|") )
        writer.write("| ")
      }
      
      def strongToLatex( children : Seq[Span] )( implicit writer : Writer ) : Unit = {
        writer.write( "\\textbf{" )
        children.foreach( toLatex )
        writer.write( "} " )
      }
      
      def emphasisToLatex( children : Seq[Span] )( implicit writer : Writer ): Unit ={
        writer.write( "\\textit{" )
        children.foreach( toLatex )
        writer.write( "} " )
      }
      
      def linkToLatex( children : Seq[Span], url : String, title : Option[String] )
                     ( implicit writer : Writer )
                     : Unit = {
        children.foreach( toLatex )
        writer.write( "\\footnote{" )
        if ( title.isDefined ) {
          writer.write( title.get )
          writer.write( " " )
        }
        writer.write( url )
        writer.write( "}" )
      }
      
      def imageLinkToLatex( children : Seq[Span], url : String,
                            title : Option[String] )
                          ( implicit writer : Writer )
                          : Unit = {
        writer.write( "\\begin{figure}\n" )
        writer.write( "\\includegraphics{" )
        writer.write( url )
        writer.write( "}" )
        writer.write( "\\caption{" )
        children.foreach( toLatex )
        writer.write( "}\n")
        writer.write( "\\end{figure}\n" )
      }
    }

    
    
## Source Files ##

    // In com/tristanhunt/knockoff/latex/LatexObjectModel.scala
    package com.tristanhunt.knockoff.latex
    
    import com.tristanhunt.knockoff.{ Span, Block }
    import scala.util.parsing.input.{ Position }
    
    // See the LaTeX object model


    // In com/tristanhunt/knockoff/latex/LatexDiscounter.scala
    package com.tristanhunt.knockoff.latex
    
    import com.tristanhunt.knockoff._
    import java.lang.{ CharSequence }
    import scala.collection.mutable.{ Buffer, ListBuffer }

    // See the LatexDiscounter


    // In com/tristanhunt/knockoff/latex/LatexWriter.scala
    package com.tristanhunt.knockoff.latex
    
    import com.tristanhunt.knockoff._
    import java.io.{ StringWriter, Writer }
      
    // See the LatexWriter