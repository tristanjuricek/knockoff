package knockoff2

object LinkMatcher extends SpanMatcher with StringExtras {
  
  def find(
      source  : String,
      convert : String => Span
    ) ( implicit
      factory : ElementFactory
    ) : Option[ SpanMatch ] = {

    normalLinks.findFirstMatchIn( source ) match {
      case None =>
        findAutomaticMatch( source, convert ).getOrElse(
          findReferenceMatch( source, convert )
        )
      case Some( matchr ) =>
        findNormalMatch( source, convert, matchr )
    }
  }
  
  /**
    If it looks like an automatic link, then it probably is one.
  */
  val automaticLink = """<([^\s>]+)>""".r
  
  def findAutomaticMatch(
      source  : String,
      convert : String => Span
    ) ( implicit
      factory : ElementFactory
    ) : Option[ SpanMatch ] = {
      
    automaticLinkRE.findFirstMatchIn( source ).map { aMatch =>
      val url = automaticLink.group(1)
      SpanMatch(
        aMatch.start,
        aMatch.before.toOption.map( text(_) ),
        link( text( url ), url, None ),
        aMatch.after.toOption
      )
    }
  }
  
  def findNormalMatch(
      source  : String,
      convert : String => Span,
      matchr  : Match
    ) ( implicit factory : ElementFactory ) : Option[ SpanMatch ] = {
     
    import factory._

    val isImage = matchr.group(1) == "!" || matchr.group(4) == "!"

    Some(
      SpanMatch(
        matchr.start,
        matchr.before.toOption.map( text(_) ),
        if ( isImage )
          
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
  
}
