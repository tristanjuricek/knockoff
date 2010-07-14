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
