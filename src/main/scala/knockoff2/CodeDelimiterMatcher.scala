package knockoff2

class CodeDelimiterMatcher(
    val delim    : String,
    val newMatch : ( Int, Option[ Text ], Span, Option[ String ], ElementFactory ) => SpanMatch
)
extends EqualDelimiterMatcher( delim, newMatch ) {
 
    override def find(
            str     : String,
            convert : String => Span
        ) ( implicit
            factory : ElementFactory
        ) : Option[ SpanMatch ] = {
        return super.find( str, source => factory.text( source ) )
    }
}
