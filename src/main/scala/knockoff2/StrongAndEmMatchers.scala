package knockoff2

trait StrongAndEmMatchers { self : EqualDelimiterMatcher with SpanConverter =>
  
    def matchUnderscoreStrongAndEm( source : String ) = {
      matchEqualDelimiters( source )( "___", createStrongAndEm, true )
    }
  
    def matchAsterixStrongAndEm( source : String ) = {
      matchEqualDelimiters( source )( "***", createStrongAndEm, true )
    }
  
    def createStrongAndEm(
      i : Int, b : Option[Text], span : Span, a : Option[ String ]
    ) = {
      import elementFactory.{ strong, em }
      SpanMatch( i, b, strong( Seq( em( span ) ) ), a )
    }
}
