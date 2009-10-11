package knockoff

trait StrongMatchers { self : EqualDelimiterMatcher with SpanConverter =>
  
  def matchUnderscoreStrong( source : String ) =
    matchEqualDelimiters( source )( "__", createStrongSpanMatch, true, Some('\\') )
  
  def matchAsterixStrong( source : String ) =
    matchEqualDelimiters( source )( "**", createStrongSpanMatch, true, Some('\\') )
  
  def createStrongSpanMatch(
    i : Int, b : Option[Text], span : Span, a : Option[ String ]
  ) = {
    SpanMatch( i, b, elementFactory.strong( span ), a )
  }
}
