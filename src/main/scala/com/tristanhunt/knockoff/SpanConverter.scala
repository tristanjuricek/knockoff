package com.tristanhunt.knockoff

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
    matchDoubleCodes, matchSingleCodes, matchLink, matchHTMLComment,
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
  
  
  def matchLink( source : String ) : Option[SpanMatch] =
    normalLinks.findFirstMatchIn( source ) match {
      case None =>
        findAutomaticMatch( source ).orElse( findReferenceMatch( source ) )
      case Some( matchr ) =>
        findNormalMatch( source, matchr )
    }
    
  private val automaticLinkRE = """<((http:|mailto:|https:)\S+)>""".r
    
  def findAutomaticMatch( source : String ) : Option[ SpanMatch ] =
    automaticLinkRE.findFirstMatchIn( source ).map { aMatch =>
      val url = aMatch.group(1)
      val before = aMatch.before.toOption.map( Text(_) )
      val link = Link( List( Text(url) ), url, None )
      SpanMatch( aMatch.start, before, link, aMatch.after.toOption )
    }
    
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
