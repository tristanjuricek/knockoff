package com.tristanhunt.knockoff2

import com.tristanhunt.knockoff2.MarkdownString._
import java.net.{ URI, URISyntaxException }
import scala.collection.mutable.{ ListBuffer }
import scala.util.parsing.combinator.{ RegexParsers }
import scala.util.parsing.input.{ Reader }

class MarkdownParser extends RegexParsers {
  
  // If you don't do this, the regex parsers will drop leading whitespace
  override def skipWhitespace = false
  
  def markdown : Parser[Block] =
    linkDefinition | paragraph | blankLine
  
  def paragraph : Parser[PlainText] =
    rep1( plainText | nonEndNewline ) <~ paragraphEnd ^^ { seq =>
      val source = ( "" /: seq.map(_.text) )( _ + _ )
      PlainText( MarkdownString.unescape( source ), source, seq.head.pos )
    }
  
  def plainText : Parser[PlainText] = new Parser[PlainText] {
    
    def apply( reader : Reader[Char] ) = {
      readUntilNewline( reader, new ListBuffer )
        .map { case ((line, rem)) =>
          Success( PlainText( unescape(line), line, reader.pos ), rem ) }
        .getOrElse( Failure( "No blank plainText", reader ) )
    }
    
    def readUntilNewline( reader : Reader[Char], cur : ListBuffer[Char] )
                : Option[ (String, Reader[Char]) ] = {
      if ( reader.atEnd || reader.first == '\n' ) {
        if ( cur.mkString("").trim.isEmpty )
          None
        else {
          if ( !reader.atEnd ) cur += reader.first
          Some( (cur.mkString, reader.rest) )
        }
      } else {
        readUntilNewline( reader.rest, cur += reader.first )
      }
    }
  }

  def paragraphEnd = new Parser[BlockEnd] {
  
    def apply( start : Reader[Char] ) = {
      
      def atEnd( reader : Reader[Char], cur : ListBuffer[Char] )
               : ParseResult[BlockEnd] = {
        if ( reader.atEnd ) {
          // locate first new line
          val break = cur.takeWhile( _ != '\n' )
          val remaining = cur.drop( break.length )
          if ( !remaining.isEmpty && remaining.head == '\n' )
            break += '\n'
          Success( BlockEnd( break.mkString, start.pos ), reader )
        } else {
          reader.first match {
            case ' '  => atEnd( reader.rest, cur += reader.first )
            case '\t' => atEnd( reader.rest, cur += reader.first )
            case '\n' =>
              cur += '\n'
              val break = cur.takeWhile( _ != '\n' )
              if ( break.count( _ == ' ') > 1 )
                Success( BlockEnd( (break += '\n').mkString, start.pos ), reader.rest )
              else {
                if ( cur.count( _ == '\n' ) > 1 ) {
                  break += '\n'
                  var rem = start
                  ( 0 until break.length ).foreach { _ => rem = rem.rest }
                  Success( BlockEnd( break.mkString, start.pos ), rem )
                } else {
                  atEnd( reader.rest, cur )
                }
              }
  
            case _ => Failure( "Non-empty line after newline", start )
          }
        }
      }
      
      atEnd( start, new ListBuffer )
    }
  }
  
  
  def nonEndNewline = new Parser[PlainText] {
    
    def apply( reader : Reader[Char] ) = {
      if ( reader.atEnd || reader.first != '\n' )
        Failure( "Not a newline", reader )
      else
        nextLine( reader.rest, new ListBuffer ) match {
          case None => Failure( "Ending newline", reader )
          case Some( line ) =>
            if ( line.trim == "" ) Failure( "Breaking newline", reader )
            else {
              var next = reader
              line.foreach { ch => next = reader.rest }
              Success( PlainText( line, line, reader.pos ), next )
            }
        }
    }
    
    def nextLine( reader : Reader[Char], cur : ListBuffer[Char] ) : Option[String] = {
      if ( reader.atEnd ) return None
      cur += reader.first
      if ( reader.first == '\n' ) return Some( cur.mkString )
      nextLine( reader.rest, cur )
    }
  }
  
  def linkDefinition : Parser[LinkDefinition] = {
    ( leadingWhitespace ~ balancedInlineBrackets ~ "[ ]*:[ ]*".r ~
      validURL ~ "[ ]*".r ~ linkTitle ~ "[ ]*\\n?".r ) ^^ {
  
      case leading ~ brackets ~ sep ~ uri ~ ws ~ title ~ nl =>
        val id = brackets.trimEnds.unescape
        val sb = new StringBuilder( leading.str )
        sb.append( brackets ).append( sep ).append( uri.toString )
          .append( ws ).append( title.map(_.original).getOrElse("") )
          .append( nl )
        LinkDefinition( uri, id, title.map(_.text), sb.toString, leading.pos )
    }
  }
  
  
  def leadingWhitespace = new Parser[StringPositional] {
    
    def apply( reader : Reader[Char] ) = checkWhitespace( reader, "", 0 )
    
    def checkWhitespace( reader : Reader[Char], cur : String, count : Int )
                       : ParseResult[StringPositional] = {
      if ( count > 3 )
        Failure( "Only 0-3 leading whitespace characters allowed", reader )
      else
        reader.first match {
          case ' ' => checkWhitespace( reader.rest, " " + cur, count + 1 )
          case _ => Success( StringPositional( cur ).setPos( reader.pos ),
                             reader )
        }
    }
  }
  
  // Should match any inline sequences within a balanced bracket sequence.
  def balancedInlineBrackets = new Parser[String] {
    def apply( reader : Reader[Char] ) = {
      reader.first match {
        case '[' => matchClose( reader.first :: Nil, reader.rest, 1 )
        case _ => Failure( "Does not start with '['", reader )
      }
    }
    
    def matchClose( cur : List[Char], reader : Reader[Char], open : Int )
                  : ParseResult[String] = { // Return type needed here (recursive)
      if ( reader.atEnd )
        Failure( "No matching bracket, open: " + open, reader.rest )
      else {
        val newCur = reader.first :: cur
        reader.first match {
          case '[' =>
            matchClose( newCur, reader.rest, open + 1 )
          case ']' =>
            if ( cur.head == '\\' )
              return matchClose( newCur, reader.rest, open )
            if ( open > 1 )
              matchClose( newCur, reader.rest, open - 1 )
            else
              Success( newCur.reverseIterator.mkString, reader.rest )
          case _ =>
            matchClose( newCur, reader.rest, open )
        }
      }
    }
  }
  
  def validURL = new Parser[URI] {
    
    def apply( reader : Reader[Char] ) = {
      val legal = legalURIString( reader, Nil )
      if ( legal.isEmpty )
        Failure( "URI is empty or starts with illegal characters", reader )
      else
        try {
          Success( new URI( legal ), reader.drop( legal.size ) )
        } catch {
          case ex : URISyntaxException => Failure( ex.getMessage, reader )
        }
    }
    
    // TODO - I wrote this in an airport, may want to review the comparison later
    // just in case, you know?
    val legalURLChars = "ABCDEFGHIJKLMOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" +
                        "0123456789_-!.~'()*,;:$&+=%?/[]@"
    
    // compare all code points: A-Z a-z 0-9 _-!.~'()*,;:$&+= % ?/[]@
    def legalURIString( reader : Reader[Char], cur : List[Char] ) : String = {
      if ( reader.atEnd ) return cur.reverse.mkString("")
      import java.lang.Character.digit
      val point = digit( reader.first, 10 )
      if ( legalURLChars contains reader.first )
        legalURIString( reader.rest, reader.first :: cur )
      else
        cur.reverse.mkString("")
    }
  }
  
  def linkTitle = new Parser[ Option[LinkTitle] ] {
  
    def apply( reader : Reader[Char] ) : ParseResult[ Option[LinkTitle] ] = {
      val (opt, rest) = reader.first match {
        case '\'' => readToClose( '\'', reader.rest, List( reader.first ), false )
        case '"'  => readToClose( '"',  reader.rest, List( reader.first ), false )
        case '('  => readToClose( ')',  reader.rest, List( reader.first ), false )
        case _    => (None, reader)
      }
      opt match {
        case Some(title) => Success( Some(title), rest )
        case None        => Success( None, reader )
      }
    }
    
    def readToClose( ch : Char, reader : Reader[Char], cur : List[Char],
                     bs : Boolean )
                   : ( Option[LinkTitle], Reader[Char] ) = {
      if ( reader.atEnd ) return (None, reader)
      val newCur = reader.first :: cur
      if ( reader.first == ch ) {
        if ( bs ) return readToClose( ch, reader.rest, newCur, false )
        val original = newCur.reverse.mkString("")
        return ( Some( LinkTitle( original, reader.pos, original.trimEnds.unescape ) ),
                 reader.rest )
      }
      readToClose( ch, reader.rest, newCur, reader.first == '\\' )
    }
  }
  
  def blankLine : Parser[EmptyBlock] = {
    leadingWhitespace ~ "\n" ^^ {
      case leading ~ nl => EmptyBlock( leading.str + nl, leading.pos )
    }
  }      
}
