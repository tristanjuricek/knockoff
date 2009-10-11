package knockoff

trait StrongAndEmMatchers { self : EqualDelimiterMatcher with SpanConverter =>
  
    def matchUnderscoreStrongAndEm( source : String ) = {
      matchEqualDelimiters( source )( "___", createStrongAndEm, true, Some('\\') )
    }
  
    def matchAsterixStrongAndEm( source : String ) = {
      matchEqualDelimiters( source )( "***", createStrongAndEm, true, Some('\\') )
    }
  
    def createStrongAndEm(
      i : Int, b : Option[Text], span : Span, a : Option[ String ]
    ) = {
      import elementFactory.{ strong, em }
      SpanMatch( i, b, strong( Seq( em( span ) ) ), a )
    }
}
