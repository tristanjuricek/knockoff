# The Markdown Generator #

This is a kind of ScalaCheck generator that generates a bunch of "interesting"
markdown block sequences, which can then be used to generate markdown for testing.

**TODO** Need to bound things tremendously.

**TODO** Take a sample of markdown usage by types from my existing code to get
a basic understanding of frequencies.

**TODO** The strong / em sequences are technically recursive, need to figure
out a nice way

**TODO** I'm not sure how to push optional data for Link, ...

**TODO** It would probably be nice to have HTML and URL generators

**TODO** Generated blocks have NoPosition... need to take this into account for
test comparisons.

**TODO** When writing out the markdown, I need to make sure I collect all
LinkDefinitions and write out any one I find at the end of the document.

    // In com/tristanhunt/knockoff/generator/MarkdownGenerator.scala
    package com.tristanhunt.knockoff.generator
    
    import com.tristanhunt.knockoff._
    import org.scalacheck.Gen._
    import org.scalacheck._
    import scala.util.parsing.input.NoPosition
    
    trait MarkdownGenerator {
      
      def word = alphaStr
      
      def newline = '\n'
      
      def blob =
        for { n <- choose(1,10); w <- listOfN(n, word); s <- value(" ") }
          yield w.mkString(s)
      
      def text =
        for { n <- choose(1,20); b <- listOfN(n, blob); nl <- value("\n") }
          yield b.mkString(nl)
      
      def textSpan =
        for ( t <- text ) yield Text(t)
      
      def html =
        value( HTMLSpan("<span>basic html</span>") )
      
      def codeSpan =
        for ( t <- text ) yield CodeSpan(t)
      
      def simpleSpan =
        oneOf( textSpan, codeSpan, html )
      
      // Could not really let things recurse due to stack overflow errors.
      def simpleSpans : Gen[Seq[Span]] =
        for { n <- choose(1,5); list <- listOfN( n, simpleSpan ) } yield list
      
      def strong =
        for ( s <- simpleSpans ) yield Strong(s)
      
      def emphasis =
        for ( s <- simpleSpans ) yield Emphasis(s)
      
      def url =
        value("http://example.com")
      
      def link =
        for ( s <- simpleSpans; u <- url; t <- blob )
          yield Link( s, u, Some(t) )
      
      def imageLink = link
      
      def indirectLink =
        for ( s <- simpleSpans; ld <- linkDefinition )
          yield IndirectLink( s, ld )
      
      def indirectImageLink =
        for ( s <- simpleSpans; ld <- linkDefinition )
          yield IndirectImageLink( s, ld )
      
      // TODO indirect links
      def spans =
        for { n <- choose(1, 5)
              span <- oneOf( textSpan, codeSpan, html, strong, emphasis, link,
                             imageLink, indirectLink, indirectImageLink )
              list <- listOfN( n, span ) }
          yield list
      
      def paragraph =
        for ( s <- spans ) yield Paragraph( s, NoPosition )
      
      def header =
        for ( lvl <- choose(1,6); s <- spans )
          yield Header( lvl, s, NoPosition )
      
      def linkDefinition =
        for ( b <- blob; u <- url; t <- blob )
          yield LinkDefinition( b, u, Some(t), NoPosition )
      
      def codeBlock =
        for ( t <- textSpan )
          yield CodeBlock( t, NoPosition )
      
      def hr =
        value( HorizontalRule(NoPosition) )
      
      def simpleBlocks =
        for { n <- choose(1, 20)
              list <- listOfN( n, oneOf( paragraph, header, linkDefinition, hr, codeBlock ) ) }
          yield list
      
      def blockquote =
        for ( b <- simpleBlocks )
          yield Blockquote( b, NoPosition )
      
      def oi =
        for ( b <- simpleBlocks )
          yield OrderedItem( b, NoPosition )
      
      def ui =
        for ( b <- simpleBlocks )
          yield UnorderedItem( b, NoPosition )
      
      def ol =
        for { n <- choose( 1, 15 )
              list <- listOfN( n, oi ) }
          yield OrderedList( list )
      
      def ul =
        for { n <- choose( 1, 15 )
              list <- listOfN( n, ui ) }
          yield UnorderedList( list )
      
      def blocks =
        for { n <- choose( 1, 50 )
              list <- listOfN( n, oneOf( paragraph, header, linkDefinition, hr, codeBlock,
                                         blockquote, ol, ul ) ) }
          yield list
    }
    
    object MarkdownGenerator extends MarkdownGenerator
