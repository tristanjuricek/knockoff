package com.tristanhunt.knockoff

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
import scala.util.logging.Logged

trait ChunkStreamFactory extends Logged {

  /** Overridable factory method. */
  def newChunkParser : ChunkParser = new ChunkParser

  lazy val chunkParser : ChunkParser = newChunkParser

  def createChunkStream( str : String ) : Stream[(Chunk, Position)] =
    createChunkStream( new CharSequenceReader( str, 0 ) )
  
  def createChunkStream( reader : Reader[Char] ) : Stream[(Chunk, Position)] = {
    if ( reader.atEnd ) return Stream.empty
    chunkParser.parse( chunkParser.chunk, reader ) match {
      case chunkParser.Error( msg, next ) => {
        log( msg )
        log( "next == reader : " + (next == reader) )
        createChunkStream( next )
      }
      case chunkParser.Failure( msg, next ) => {
        log( msg )
        log( "next == reader : " + (next == reader) )
        createChunkStream( next )
      }
      case chunkParser.Success( result, next ) => {
        Stream.cons( ( result, reader.pos ), createChunkStream( next ) )
      }
    }
  }
}

import scala.util.parsing.combinator.RegexParsers

class ChunkParser extends RegexParsers with StringExtras {
    
  override def skipWhitespace = false
  
  def chunk : Parser[ Chunk ] = {
    horizontalRule | leadingStrongTextBlock | leadingEmTextBlock | bulletItem |
    numberedItem | indentedChunk | header | blockquote | linkDefinition |
    textBlockWithBreak | textBlock | emptyLines
  }
  
  def emptyLines : Parser[ Chunk ] =
    rep1( emptyLine ) ^^ ( str => EmptySpace( foldedString( str ) ) )
  
  def emptyLine : Parser[ Chunk ] =
    """[\t ]*\r?\n""".r ^^ ( str => EmptySpace( str ) )

  def textBlockWithBreak : Parser[ Chunk ] =
    rep( textLineWithEnd ) ~ hardBreakTextLine ^^ { case seq ~ break => TextChunk( foldedString(seq) + break.content ) }

  def textBlock : Parser[ Chunk ] =
    rep1( textLine ) ^^ { seq => TextChunk( foldedString(seq) ) }
  
  /** Match any line up until it ends with a newline. */
  def textLine : Parser[ Chunk ] =
    """[\t ]*\S[^\n]*\n?""".r ^^ { str => TextChunk(str) }

  def textLineWithEnd : Parser[Chunk] =
    """[\t ]*\S[^\n]*[^ \n][ ]?\n""".r ^^ { str => TextChunk(str) }
  
  def hardBreakTextLine : Parser[Chunk] =
    """[\t ]*\S[^\n]*[ ]{2}\n""".r ^^ { s => TextChunk(s) }

  def bulletItem : Parser[ Chunk ] =
    bulletLead ~ rep( trailingLine ) ^^ {
      case ~(lead, texts) => BulletLineChunk( foldedString( lead :: texts ) ) }
  
  /** Match a single line that is likely a bullet item. */
  def bulletLead : Parser[ Chunk ] =
    // """[ ]{0,3}[*\-+](\t|[ ]{0,4})""".r ~> not("[*\\-+]".r) ~> textLine ^^ {
    """[ ]{0,3}[*\-+](\t|[ ]{0,4})""".r ~> textLine ^^ {
      textChunk => BulletLineChunk( textChunk.content ) }
  
  /** A special case where an emphasis marker, using an asterix, on the first word
      in a text block doesn't make the block a list item. We'll only catch lines
      here that have an even number of asterixes, because if it's odd, well, you
      probably have an asterix line indicator followed by an emphasis. */
  def leadingEmTextBlock : Parser[ Chunk ] =
    """[ ]{0,3}\*""".r ~ notEvenAsterixes ~ rep( textLine ) ^^ {
      case ~(~(emLine, s), textSeq) => TextChunk( emLine + s + foldedString(textSeq) ) }
  
  def notEvenAsterixes = new Parser[String] {

    def apply( in : Reader[Char] ) : ParseResult[String] = {
      val (line, asterixCount, remaining) = readLine( in, new StringBuilder, 0 )
      if ( asterixCount >= 1 && asterixCount % 2 == 1 ) return Success( line, remaining )
      else Failure( "Odd number of asterixes, skipping.", in )
    }
    
    def readLine( in : Reader[Char], sb : StringBuilder, count : Int )
                : (String, Int, Reader[Char]) = {
      if ( ! in.atEnd ) sb.append( in.first )
      if ( in.atEnd || in.first == '\n' ) return (sb.toString, count, in.rest)
      if ( in.first == '*' ) readLine( in.rest, sb, count + 1 )
      else readLine( in.rest, sb, count )
    }
  }
      
  /** A special case where an emphasis marker on a word on a text block doesn't
      make the block a list item. */
  def leadingStrongTextBlock : Parser[ Chunk ] =
    """[ ]{0,3}\*\*[^*\n]+\*\*[^\n]*\n?""".r ~ rep( textLine ) ^^ {
      case ~(strLine, textSeq) => TextChunk( strLine + foldedString(textSeq) ) }
  
  def numberedItem : Parser[ Chunk ] =
    numberedLead ~ rep( trailingLine ) ^^ {
      case ~(lead, texts) => NumberedLineChunk( foldedString( lead :: texts )) }
  
  def numberedLead : Parser[ Chunk ] =
    """[ ]{0,3}\d+\.(\t|[ ]{0,4})""".r ~> textLine ^^ {
      textChunk => NumberedLineChunk( textChunk.content ) }
  
  def trailingLine : Parser[ Chunk ] =
    """\t|[ ]{0,4}""".r ~> """[\S&&[^*\-+]&&[^\d]][^\n]*\n?""".r ^^ (
      s => TextChunk(s) )
  
  def header : Parser[ Chunk ] =
    ( setextHeaderEquals | setextHeaderDashes | atxHeader )

  def setextHeaderEquals : Parser[ Chunk ] =
    textLine <~ equalsLine ^^ ( s => HeaderChunk( 1, s.content.trim ) )

  def setextHeaderDashes : Parser[ Chunk ] =
    textLine <~ dashesLine ^^ ( s => HeaderChunk( 2, s.content.trim ) )

  def equalsLine : Parser[Any] = """=+\n""".r

  def dashesLine : Parser[Any] = """-+\n""".r

  def atxHeader : Parser[ Chunk ] =
    """#+ .*\n?""".r ^^ (
      s => HeaderChunk( s.countLeading('#'), s.trimChars('#').trim ) )
  
  def horizontalRule : Parser[ Chunk ] =
    """[ ]{0,3}[*\-_][\t ]?[*\-_][\t ]?[*\-_][\t *\-_]*\n""".r ^^ {
      s => HorizontalRuleChunk }
  
  def indentedChunk : Parser[ Chunk ] =
    rep1( indentedLine ) ^^ ( lines => IndentedChunk( foldedString( lines ) ) )
  
  def indentedLine : Parser[ Chunk ] =
    """\t|[ ]{4}""".r ~> ( textLine | emptyLine | emptyString )

  def emptyString : Parser[ Chunk ] = "".r ^^ ( s => EmptySpace(s) )
  
  def blockquote : Parser[ Chunk ] =
    blockquotedLine ~ rep( blockquotedLine | textLine ) ^^ {
      case ~(lead, trailing) =>
        BlockquotedChunk( foldedString( lead :: trailing ) ) }
  
  def blockquotedLine : Parser[ Chunk ] =
    """^>[\t ]?""".r ~> ( textLine | emptyLine )

  def linkDefinition : Parser[ Chunk ] =
    linkIDAndURL ~ opt( linkTitle ) <~ """[ ]*\n?""".r ^^ {
      case ~( idAndURL, titleOpt ) =>
        LinkDefinitionChunk( idAndURL._1, idAndURL._2, titleOpt ) }

  private def linkIDAndURL : Parser[ (String, String) ] =
    """[ ]{0,3}\[[^\[\]]*\]:[ ]+<?[\w\p{Punct}]+>?""".r ^^ { linkString =>
      val linkMatch = """^\[([^\[\]]+)\]:[ ]+<?([\w\p{Punct}]+)>?$""".r
                        .findFirstMatchIn( linkString.trim ).get;
      ( linkMatch.group(1), linkMatch.group(2) )
    }

  private def linkTitle : Parser[ String ] =
    """\s*""".r ~> """["'(].*["')]""".r ^^ ( // " <- My TextMate bundle fails here
      str => str.substring( 1, str.length - 1 ) )
  
  // Utility Methods
  
  /** Take a series of very similar chunks and group them. */
  private def foldedString( texts : List[ Chunk ] ) : String =
    ( "" /: texts )( (current, text) => current + text.content )
}

import scala.collection.mutable.{ Buffer, ListBuffer }

trait Chunk {
  def content : String
  def isLinkDefinition = false

  /** Create the Block and append to the list. */
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter )
}

case class BlockquotedChunk( content : String ) extends Chunk {

  /** @param content The material, not parsed, but also not containing this
                     level's '>' characters. */
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    val blocks = discounter.knockoff( content )
    list += Blockquote( blocks, position )
  }
}

case class EmptySpace( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    if ( remaining.isEmpty ) return
    if ( list.isEmpty ) return
    list.last match {
      case lastCB : CodeBlock =>
        remaining.head._1 match {
          case ice : IndentedChunk =>
            list.update( list.length - 1,
                         CodeBlock( Text( lastCB.text.content + "\n" ),
                                    lastCB.position ) )
          case _ => {}
        }
      case _ => {}
    }
  }
}

case class HeaderChunk( val level : Int, val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += Header( level, spans, position )
  }
}

case object HorizontalRuleChunk extends Chunk {
  val content = "* * *\n"
  
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += HorizontalRule( position )
  }
}

case class IndentedChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    if ( list.isEmpty ) {
      spans.first match {
        case text : Text => list += CodeBlock( text, position )
      }
    } else {
      list.last match {
        case OrderedList( items ) =>
          val blocks = discounter.knockoff( content )
          val li = OrderedItem( items.last.children ++ blocks, items.last.position )
          list.update( list.length - 1, OrderedList( items.take( items.length - 1) ++ List(li) ) )

        case UnorderedList( items ) =>
          val blocks = discounter.knockoff( content )
          val li =
            UnorderedItem( items.last.children ++ blocks, items.last.position )
          list.update( list.length - 1, UnorderedList( items.take( items.length - 1) ++ List(li) ) )

        case CodeBlock( text, position ) =>
          spans.first match {
            case next : Text =>
              list.update( list.length - 1,
                           CodeBlock(Text(text.content + next.content), position) )
          }
        
        case _ =>
          spans.first match {
            case text : Text => list += CodeBlock( text, position )
          }
      }
    }
  }
}

case class LinkDefinitionChunk( val id : String, val url : String,
                                val title : Option[ String ] )
extends Chunk {

  override def isLinkDefinition = true
  
  def content : String =
    "[" + id + "]: " + url + title.map( " \"" + _ + "\"" ).getOrElse("")
  
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += LinkDefinition( id, url, title, position )
  }
}

case class NumberedLineChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    val li = OrderedItem( List(Paragraph(spans, position)), position )
    if ( list.isEmpty ) {
      list += OrderedList( List(li) )
    } else {
      list.last match {
        case ol : OrderedList =>
          list.update( list.length - 1, OrderedList( ol.items ++ List(li) ) )
        case _ => list += OrderedList( List(li) )
      }
    }
  }
}

case class TextChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    def appendList = list += Paragraph(spans, position)
	
    if (list.isEmpty) {
    	appendList
    } else {
		list.last match {
			case p : Paragraph =>
			  if (endsWithBreak(p.spans)) {
			 	list.trimEnd(1)
			    list += appendBreakAndSpans(p.spans, spans, position)
			  }
			  else appendList
					
			case _ => appendList
		}
    }
  }
  
  def endsWithBreak(spans : Seq[Span]) : Boolean = {
    if (spans.isEmpty) return false
    spans.last match {
    	case text : Text =>
    		text.content.endsWith("  \n")
    	case _ => false
    }
  }
  
  def appendBreakAndSpans(preSpans : Seq[Span], tailSpans : Seq[Span],
		                  position : Position) : Paragraph = {
	Paragraph(preSpans ++ List(HTMLSpan("<br/>\n")) ++ tailSpans, position)
  }
}

case class BulletLineChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    val li = UnorderedItem( List(Paragraph(spans, position)), position )
    if ( list.isEmpty ) {
      list += UnorderedList( List(li) )
    } else {
      list.last match {
        case ul : UnorderedList =>
          list.update( list.length - 1, UnorderedList( ul.items ++ List(li) ) )
        case _ => list += UnorderedList( List(li) )
      }
    }
  }
}

import scala.util.matching.Regex.Match

class SpanConverter( definitions : Seq[LinkDefinitionChunk] )
extends Function1[ Chunk, Seq[Span] ] with StringExtras {

  case class SpanMatch( index : Int, before : Option[Text], current : Span,
                        after : Option[ String ] )

  
  /** @param delim The delimiter string to match the next 2 sequences of.
      @param toSpanMatch Factory to create the actual SpanMatch.
      @param recursive If you want the contained element to be reconverted.
      @param escape If set, how you can escape this sequence. */
  class DelimMatcher( delim : String, toSpan : Seq[Span] => Span,
                      recursive : Boolean, escape : Option[Char] )
  extends Function1[ String, Option[SpanMatch] ] {
    
    def apply( source : String ) : Option[SpanMatch] = {
      
      source.nextNIndicesOf( 2, delim, escape ) match {
        case List( start, end ) =>
          if ( start + delim.length >= end ) return None
          val contained = source.substring( start + delim.length, end )
          val content = if ( recursive ) convert( contained, Nil )
                        else List( Text(contained) )
          val before = source.substringOption( 0, start ).map( Text(_) )
          val after = source.substringOption( end + delim.length, source.length )
          val mapped = toSpan( content )
          Some( SpanMatch( start, before, mapped, after ) )
        case _ => None
      }
    }
  }

  def apply( chunk : Chunk ) : Seq[Span] = {
    chunk match {
      case IndentedChunk( content ) => List( new Text(content) )
      case _ => convert( chunk.content, Nil )
    }
  }
  
  /** Tail-recursive method halts when the content argument is empty. */
  protected def convert( content : String, current : List[Span] ) : Seq[Span] = {

    if ( content.isEmpty ) return current

    val textOnly = SpanMatch( content.length, None, Text( content ), None )

    val best = ( textOnly /: matchers ){ (current, findMatch) =>
      findMatch( content ) match {
        case None => current
        case Some( nextMatch ) =>
          if ( nextMatch.index < current.index ) nextMatch
          else current
      }
    }
  
    val updated = current ::: best.before.toList ::: List( best.current )
  
    best.after match {
      case None              => updated
      case Some( remaining ) => convert( remaining, updated )
    }
  }
  
  def matchers : List[ String => Option[SpanMatch] ] = List(
    matchDoubleCodes, matchSingleCodes, findReferenceMatch, findAutomaticMatch,
    findNormalMatch, matchHTMLComment,
    matchEntity, matchHTMLSpan, matchUnderscoreStrongAndEm,
    matchAsterixStrongAndEm, matchUnderscoreStrong, matchAsterixStrong,
    matchUnderscoreEmphasis, matchAsterixEmphasis
  )
  
  val matchUnderscoreEmphasis =
    new DelimMatcher( "_", Emphasis(_), true, Some('\\') )
  
  val matchAsterixEmphasis =
    new DelimMatcher( "*", Emphasis(_), true, Some('\\') )
  
  
  val matchUnderscoreStrong =
      new DelimMatcher( "__", Strong(_), true, Some('\\') )
  
  val matchAsterixStrong =
      new DelimMatcher( "**", Strong(_), true, Some('\\') )
  
  
  val matchUnderscoreStrongAndEm = 
    new DelimMatcher( "___", seq => Strong( List(Emphasis(seq)) ), true,
                      Some('\\') )
  
  val matchAsterixStrongAndEm =
    new DelimMatcher( "***", seq => Strong( List(Emphasis(seq)) ), true,
                      Some('\\') )
  
  val matchDoubleCodes =
    new DelimMatcher( "``", s => CodeSpan( s.first.asInstanceOf[Text].content ),
                      false, None )
  
  val matchSingleCodes = 
    new DelimMatcher( "`", s => CodeSpan( s.first.asInstanceOf[Text].content ),
                      false, None )
    
 
  private val startElement = """<[ ]*([a-zA-Z0-9:_]+)[ \t]*[^>]*?(/?+)>""".r
  
  def matchHTMLSpan( source : String ) : Option[SpanMatch] = {
    startElement.findFirstMatchIn( source ).map { open =>
      val hasEnd = open.group(2) == "/"
      val before = open.before.toOption.map( Text(_) )
      val noEnd = SpanMatch( open.start, before, HTMLSpan( open.matched ),
                             open.after.toOption )
      if ( ! hasEnd ) {
        hasMatchedClose( source, open.group(1), open.end, 1 ) match {
          case Some((close, after)) =>
            val before = open.before.toOption.map( Text(_) )
            val html = HTMLSpan( source.substring( open.start, close ) )
            SpanMatch( open.start, before, html, after.toOption )
          // Let no html-like thing go unpunished.
          case None => noEnd
        }
      } else {
        noEnd
      }
    }
  }
  
  private def hasMatchedClose( source : String, tag : String, from : Int,
                               opens : Int )
                             : Option[ (Int, CharSequence) ] = {
  
    val opener = ("(?i)<[ ]*" + tag + "[ \t]*[^>]*?(/?+)*>").r
    val closer = ("(?i)</[ ]*" + tag + "[ ]*>").r
    
    val nextOpen  = opener.findFirstMatchIn( source.substring(from) )
    val nextClose = closer.findFirstMatchIn( source.substring(from) )
  
    if ( ! nextClose.isDefined ) return None
    
    if ( nextOpen.isDefined && ( nextOpen.get.start < nextClose.get.start ) ) {
      hasMatchedClose( source, tag, from + nextOpen.get.end, opens + 1 )
    } else if ( opens > 1 ) {
      hasMatchedClose( source, tag, from + nextClose.get.end, opens - 1 )
    } else {
      Some( ( from + nextClose.get.end, nextClose.get.after ) )
    }
  }
  
  private val matchEntityRE = """&\w+;""".r
  
  def matchEntity( source : String ) : Option[ SpanMatch ] =
    matchEntityRE.findFirstMatchIn( source ).map { entityMatch =>
      val before = entityMatch.before.toOption.map( Text(_) )
      val html = HTMLSpan( entityMatch.matched )
      SpanMatch( entityMatch.start, before, html, entityMatch.after.toOption )
    }
  
  def matchHTMLComment( source : String ) :Option[ SpanMatch ] = {
    val open = source.indexOf("<!--")
    if ( open > -1 ) {
      val close = source.indexOf( "-->", open )
      if ( close > -1 ) {
        val before = source.substring( 0, open ).toOption.map( Text(_) )
        val html = HTMLSpan( source.substring( open, close + "-->".length ) )
        val after = source.substring( close + "-->".length ).toOption
        return Some( SpanMatch( open, before, html, after ) )
      }
    }
    return None
  }
  
  
  private val automaticLinkRE = """<((http:|mailto:|https:)\S+)>""".r
  
  def findAutomaticMatch( source : String ) : Option[ SpanMatch ] =
    automaticLinkRE.findFirstMatchIn( source ).map { aMatch =>
      val url = aMatch.group(1)
      val before = aMatch.before.toOption.map( Text(_) )
      val link = Link( List( Text(url) ), url, None )
      SpanMatch( aMatch.start, before, link, aMatch.after.toOption )
    }
  
  def findNormalMatch( source : String ) : Option[SpanMatch] =
    normalLinks.findFirstMatchIn( source )
               .flatMap { matchr => findNormalMatch( source, matchr ) }
  
  def findNormalMatch( source : String, matchr : Match ) : Option[ SpanMatch ] = {
    val isImage     = matchr.group(1) == "!" || matchr.group(4) == "!"
    val hasTitle    = matchr.group(7) != null
    val wrapped     = if( hasTitle ) matchr.group(5) else matchr.group(2)
    val url         = if( hasTitle ) matchr.group(6) else matchr.group(3)
    val titleOption = if ( hasTitle ) Some( matchr.group(7) ) else None
    val before = matchr.before.toOption.map( Text(_) )
    val link = if ( isImage ) ImageLink( convert(wrapped, Nil), url, titleOption )
               else Link( convert(wrapped, Nil), url, titleOption )
    Some( SpanMatch( matchr.start, before, link, matchr.after.toOption ) )
  }
    
  val normalLinks =
    ( """(!?)\[([^\]]*)\][\t ]*\(<?([\S&&[^)>]]*)>?\)|""" +
      """(!?)\[([^\]]*)\][\t ]*\(<?([\S&&[^)>]]*)>?[\t ]+"([^)]*)"\)""" ).r
  
    
  /** We have to match parens, to support this stuff: [wr [app] ed] [thing] */
  def findReferenceMatch( source : String ) : Option[SpanMatch] = {
    val firstOpen = source.indexOf('[')
    if ( firstOpen == -1 ) return None
    
    val firstClose =
      source.findBalanced('[', ']', firstOpen).getOrElse( return None )
  
    val secondPart = source.substring( firstClose + 1 )
  
    val secondMatch =
      """^\s*(\[)""".r.findFirstMatchIn( secondPart ).getOrElse( return None )
  
    val secondClose =
      secondPart.findBalanced( '[', ']', secondMatch.start(1) ).get
    if ( secondClose == -1 ) return None
  
    val refID = {
      val no2 = secondPart.substring( secondMatch.start(1) + 1, secondClose )
      if ( no2.isEmpty ) source.substring( firstOpen + 1, firstClose ) else no2
    }
    val precedingText = source.substring( 0, firstOpen ).toOption.map( Text(_) )
    
    definitions.find( _.id equalsIgnoreCase refID ).map {
      definition : LinkDefinitionChunk =>
        val link = Link( List( Text(source.substring(firstOpen + 1, firstClose)) ),
                         definition.url, definition.title )
        val after = source.substring( firstClose + secondClose + 2 ).toOption
        SpanMatch( firstOpen, precedingText, link, after )
    }
  }
}
