package knockoff2

trait StrongMatchers { self : EqualDelimiterMatcher with SpanConverter =>
  
  def matchUnderscoreStrong( source : String ) =
    matchEqualDelimiters( source )( "__", createStrongSpanMatch, true )
  
  def matchAsterixStrong( source : String ) =
    matchEqualDelimiters( source )( "**", createStrongSpanMatch, true )
  
  def createStrongSpanMatch(
    i : Int, b : Option[Text], span : Span, a : Option[ String ]
  ) = {
    SpanMatch( i, b, elementFactory.strong( span ), a )
  }
}
