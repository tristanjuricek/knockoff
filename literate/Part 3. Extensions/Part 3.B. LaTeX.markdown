# Part 3.B. LaTeX #

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



## Object Model Extensions For LaTeX ##

Extensions for LaTeX in the markdown are simple. The LaTeX-specific markup is passed
through as strings.

    // The LaTeX object model
    case class LatexSpan( latex : String ) extends Span
    
    case class LatexBlock( latex : String, position : Position ) extends Block



## LaTeX Discovery ##

These adjust the `knockoff` method in a post-processing step to capture the LaTeX
in the Markdown document.

    // The LatexDiscounter
    trait LatexDiscounter extends Discounter {
      
      override def knockoff( source : CharSequence ) : Seq[Block] =
        super.knockoff( source ).map( toLatex )
      
      override def createSpanConverter( linkDefinitions : Seq[LinkDefinitionChunk] ) =
        new SpanConverter( linkDefinitions ) with LatexSpanConverter

      def toLatex( block : Block ) : Block = block match {
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
      
      /** If the first real line begins with \begin and last real line begins with
        * \end, it's a latex block expression. */
      def isLatex( code : String ) : Boolean = {
        val lines = code.split("\n").filter( ! _.trim.isEmpty ).toList
        return ( 2 <= lines.length &&
                 lines.head.trim.startsWith("\\begin") &&
                 lines.last.trim.startsWith("\\end") )
      }
    }

#### LatexSpanConverter

    // The LatexSpanConverter
    trait LatexSpanConverter extends SpanConverter {
      
      override def matchers = matchLatexSpan :: super.matchers
      
      val matchLatexSpan =
        new DelimMatcher( "$", s => LatexSpan( "$" + s.first.asInstanceOf[Text].content + "$" ),
                          false, Some('\\') )
      
    }


#### LatexDiscounterSpec.scala

    // In test com/tristanhunt/knockoff/extra/LatexDiscounterSpec.scala
    package com.tristanhunt.knockoff.extra
    
    import com.tristanhunt.knockoff._
    import org.scalatest.fixture.FixtureFlatSpec
    import org.scalatest.matchers.MustMatchers
    
    class LatexDiscounterSpec extends FixtureFlatSpec with MustMatchers {
      
      behavior of "LatexDiscounter"
      
      type FixtureParam = LatexDiscounter
      
      class TestLatexDiscounter extends LatexDiscounter
      
      def withFixture( test : OneArgTest ) {
        test( new TestLatexDiscounter )
      }

      private def spanList( discounter : Discounter, src : String ) : List[Span] = {
        val blocks = discounter.knockoff( "A series $s_1, s_2$ is simple" )
        val para = blocks.first.asInstanceOf[Paragraph]
        return para.spans.toList
      }
      
      it should "read latex with underscores" in { discounter =>
        val actual = spanList( discounter, "A series $s_1, s_2$ is simple" )
        val expected = List( Text("A series "),
                             LatexSpan("$s_1, s_2$"),
                             Text(" is simple") )
        actual must equal ( expected )
      }
    }



## Latex HTML Output ##

I'm using [SnuggleTex](http://www2.ph.ed.ac.uk/snuggletex) to output the LaTeX
fragments into MathML for now.

    // The LatexXHTMLWriter
    trait LatexXHTMLWriter extends XHTMLWriter {

      override def blockToXHTML : Block => Node = block => block match {
        case LatexBlock( tex, _ ) => toMathML( tex )
        case _ => super.blockToXHTML( block )
      }
      
      override def spanToXHTML : Span => Node = span => span match {
        case LatexSpan( tex ) => toMathML( tex )
        case _ => super.spanToXHTML( span )
      }
      
      def toMathML( tex : String ) : Node = {
        val engine = new SnuggleEngine
        val session = engine.createSession
        val input = new SnuggleInput(tex)
        session.parseInput(input)
        return Unparsed( session.buildXMLString )
      }
    }

#### LatexXHTMLWriterSpec.scala

Right now tidy is removing the MathML output, so the verification has do be done
inline, in a very basic manner.

    // In test com/tristanhunt/knockoff/extra/LatexXHTMLWriterSpec.scala
    package com.tristanhunt.knockoff.extra
    
    import org.scalatest.fixture.FixtureFlatSpec
    import org.scalatest.matchers.MustMatchers
    
    class LatexXHTMLWriterSpec extends FixtureFlatSpec with MustMatchers {
      
      behavior of "LatexXHTMLWriter"
      
      type FixtureParam = LatexDiscounter with LatexXHTMLWriter
      
      class TestDiscounter extends LatexDiscounter with LatexXHTMLWriter
      
      def withFixture( test : OneArgTest ) {
        test( new TestDiscounter )
      }
      
      it should "write out MathML inside a paragraph" in { discounter =>
        val actual = htmlString( discounter, "A sequence $s_1, s_2 = N * M$ is *cool*." )
        actual must equal (
          "<p>A sequence " +
          "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" +
            "<msub><mi>s</mi><mn>1</mn></msub><mo>,</mo>" +
            "<msub><mi>s</mi><mn>2</mn></msub><mo>=</mo>" +
            "<mi>N</mi><mo>*</mo><mi>M</mi></math>" +
          " is <em>cool</em>.</p>"  )
      }
      
      private def htmlString( discounter : FixtureParam, src : String ) : String = {
        val blocks = discounter.knockoff( src )
        val xhtml = discounter.toXHTML( blocks )
        val writer = new java.io.StringWriter
        scala.xml.XML.write( writer, xhtml, "utf-8", false, null )
        return writer.toString
      }
    }


## Writing LaTeX ##

This enables a slightly different call:

    import discounter._
    toLatex( knockoff( markdownString ) )

Since it's just string building, you can alternatively pass in your own `Writer`.

Usage of these files will probably be as fragments included in other documents, just
like XHTML usage. That means you won't get all the heading front matter, etc.

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
    trait LatexWriter extends XHTMLWriter {
      
      override def blockToXHTML : Block => Node = block => block match {
        case LatexBlock( latex, _ ) => XMLText( latex )
        case _ => super.blockToXHTML( block )
      }
      
      override def spanToXHTML : Span => Node = span => span match {
        case LatexSpan( latex ) => XMLText( latex )
        case _ => super.spanToXHTML( span )
      }
      
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



#### Latex.scala

    // In com/tristanhunt/knockoff/extra/Latex.scala
    package com.tristanhunt.knockoff.extra
    
    import com.tristanhunt.knockoff._
    import scala.util.parsing.input.{ Position }
    
    // See the LaTeX object model
    
    import java.lang.{ CharSequence }
    import scala.collection.mutable.{ Buffer, ListBuffer }
    
    // See the LatexDiscounter
    
    import uk.ac.ed.ph.snuggletex.{ SnuggleEngine, SnuggleInput }
    
    import scala.xml.{ Node, Text => XMLText, Unparsed }
    
    // See the LatexXHTMLWriter
    
    import java.io.{ StringWriter, Writer }
        
    // See the LatexWriter
    
    // See the LatexSpanConverter