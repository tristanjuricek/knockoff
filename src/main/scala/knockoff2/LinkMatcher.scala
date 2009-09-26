package knockoff2

import scala.util.matching.Regex.Match

trait LinkMatcher { self : SpanConverter =>
  
  def matchLink( source : String ) : Option[ SpanMatch ] = {
    normalLinks.findFirstMatchIn( source ) match {
      case None =>
        findAutomaticMatch( source ).orElse( findReferenceMatch( source ) )
      case Some( matchr ) =>
        findNormalMatch( source, matchr )
    }
  }
  
  private val automaticLinkRE = """<([^\s>]+)>""".r
  
  def findAutomaticMatch( source : String ) : Option[ SpanMatch ] = {
    automaticLinkRE.findFirstMatchIn( source ).map { aMatch =>
      val url = aMatch.group(1)
      SpanMatch(
        aMatch.start,
        aMatch.before.toOption.map( elementFactory.text(_) ),
        elementFactory.link( elementFactory.text( url ), url, None ),
        aMatch.after.toOption
      )
    }
  }
  
  def findNormalMatch( source : String, matchr : Match )
  : Option[ SpanMatch ] = {
    val isImage     = matchr.group(1) == "!" || matchr.group(4) == "!"
    val hasTitle    = matchr.group(7) != null
    val wrapped     = if( hasTitle ) matchr.group(5) else matchr.group(2)
    val url         = if( hasTitle ) matchr.group(6) else matchr.group(3)
    val titleOption = if ( hasTitle ) Some( matchr.group(7) ) else None
    Some(
      SpanMatch(
        matchr.start,
        matchr.before.toOption.map( elementFactory.text(_) ),
        if ( isImage )
          elementFactory.ilink( convert( wrapped, Nil ), url, titleOption )
        else
          elementFactory.link( convert( wrapped, Nil ), url, titleOption ),
        matchr.after.toOption
      )
    )
  }
  
  /**
    This regular expression will try to match links like: [wrap](url) and
    [wrap](url "title"), in image mode or not.
    
    Groups:
    <ul>
    <li> 1 - "!" for image, no title </li>
    <li> 2 - wrapped content, no title </li>
    <li> 3 - url, no title </li>
    <li> 4 - "!" for image, with title </li>
    <li> 5 - wrapped content, with title </li>
    <li> 6 - url, with title </li>
    <li> 7 - title </li>
    </ul>
  */
  val normalLinks = (
    """(!?)\[([^\]]*)\][ ]*\(<?([\S&&[^)>]]*)>?\)|""" +
    """(!?)\[([^\]]*)\][ ]*\(<?([\S&&[^)>]]*)>?[ ]+"([^)]*)"\)"""
  ).r
  
  /**
    We have to match parens, to support this stuff: [wr [app] ed] [thing]
  */
  def findReferenceMatch( source : String ) : Option[ SpanMatch ] = {
    val firstOpen = source.indexOf('[')
    if ( firstOpen == -1 ) return None
    
    val firstClose =
      source.findBalanced('[', ']', firstOpen).getOrElse( return None )

    val secondPart = source.substring( firstClose + 1 )

    val secondMatch =
      """^\s*(\[)""".r.findFirstMatchIn( secondPart ).getOrElse( return None )

    val secondClose = secondPart.findBalanced('[', ']', secondMatch.start).get
    if ( secondClose == -1 ) return None

    val refID = secondPart.substring( secondMatch.start + 1, secondClose )
    val precedingText = source.substring( 0, firstOpen ).toOption.map(
      elementFactory.text(_)
    )
    
    definitions.find( _.id == refID ).map { definition : LinkDefinition =>
      SpanMatch(
        firstOpen,
        precedingText,
        elementFactory.link(
          elementFactory.text( source.substring( firstOpen + 1, firstClose ) ),
          definition.url,
          definition.title
        ),
        source.substring( firstClose + secondClose + 2 ).toOption
      )
    }
  }
}
