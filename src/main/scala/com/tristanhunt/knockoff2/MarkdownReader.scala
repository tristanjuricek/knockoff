package com.tristanhunt.knockoff2

import scala.util.parsing.combinator.{ RegexParsers }
import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }

abstract class MarkdownReader(
                  val markdownParser : MarkdownParser = new MarkdownParser )
extends        LoggedParser {

  def readMarkdown( reader : Reader[Char] ) : Stream[Block] = {
    if ( reader.atEnd ) return Stream.empty
    
    import markdownParser._
    parse( markdown, reader ) match {
      case Error( msg, next ) =>
        logParsingError( msg, next )
        Stream.cons( PlainText( next.first.toString, next.first.toString, reader.pos ),
                     readMarkdown( next.rest ) )
      case Failure( msg, next ) =>
        logParsingError( msg, next )
        Stream.empty
      case Success( result, next ) =>
        Stream.cons( result, readMarkdown( next ) )
    }
  }
  
  def readMarkdown( str : String ) : Stream[Block] =
    readMarkdown( new CharSequenceReader( str ) )    
}
