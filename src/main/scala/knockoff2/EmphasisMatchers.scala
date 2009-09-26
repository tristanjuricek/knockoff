package knockoff2

trait EmphasisMatchers { self : EqualDelimiterMatcher with SpanConverter =>
 
  def matchUnderscoreEmphasis( source : String ) =
    matchEqualDelimiters( source )( "_", createEmphasisSpanMatch, true )

  def matchAsterixEmphasis( source : String ) =
    matchEqualDelimiters( source )( "*", createEmphasisSpanMatch, true )

  def createEmphasisSpanMatch(
    i : Int, b : Option[Text], span : Span, a : Option[ String ]
  ) = {
    SpanMatch( i, b, elementFactory.em( span ), a )
  }
}
