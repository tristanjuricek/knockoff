package knockoff2 

object LinkMatcher extends SpanMatcher {
  
  /**
    This regular expression will try to match all of the "two bracket" types we
    want. It will also create an optional group for the image reference tag.
    We're only looking for the start of any image reference, and likely match,
    not exact.
  */
  val normalLinks = """(!?)\[[^\]]*\][\[(][^\])]*[\])]""".r
  
  def find(
      source  : String,
      convert : String => Span
    ) ( implicit
      factory : ElementFactory
    ) : Option[ SpanMatch ] = {

    normalLinks.findFirstMatchIn( source ) match {
      case None => findAutomaticMatch( source, convert )
      case Some( match ) =>
        findNormalMatch( source, convert, match, match.group(1) == "!" )
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
      SpanMatch( aMatch.start, 
    }
  }
}
