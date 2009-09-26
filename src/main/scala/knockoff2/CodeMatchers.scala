package knockoff2

trait CodeMatchers { self : EqualDelimiterMatcher with SpanConverter =>
 
  def matchDoubleCodes( source : String ) : Option[ SpanMatch ] =
    matchEqualDelimiters( source )( "``", createCodeSpanMatch, false )

  def matchSingleCodes( source : String ) : Option[ SpanMatch ] =
    matchEqualDelimiters( source )( "`", createCodeSpanMatch, false )
  
  def createCodeSpanMatch(
    i : Int, b : Option[Text], span : Span, a : Option[ String ]
  ) = {
    val codeSpan = span match {
      case text : Text => elementFactory.codeSpan( text.content )
    }
    SpanMatch( i, b, codeSpan, a )
  }
}
