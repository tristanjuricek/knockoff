package knockoff2

import scala.util.logging.Logged

class   EqualDelimiterMatcher(
  delim    : String,
  newMatch : ( Int, Option[ Text ], Span, Option[ String ], ElementFactory ) => 
               SpanMatch
)
extends SpanMatcher
with    StringExtras {
    
  def find(
      str     : String,
      convert : String => Span
    ) ( implicit
      factory : ElementFactory
    ) : Option[ SpanMatch ] = {
 
    import factory._
 
    str.nextNIndicesOf( 2, delim ) match {
      case List( start, finish ) => {
        Some( newMatch(
          start,
          str.substringOption( 0, start ).map( text ),
          convert( str.substring( start + delim.length, finish ) ),
          str.substringOption( finish + delim.length, str.length ),
          factory
        ) )
      }
      case _ => None
    }
  }
}
