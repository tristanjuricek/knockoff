package knockoff2

class SpanConverter(
    val definitions : Seq[ LinkDefinition ],
    val matchers    : Seq[ SpanMatcher ],
    val factory     : ElementFactory
)
extends Function1[ Chunk, SpanSeq ] {
 
    def apply( chunk : Chunk ) : SpanSeq =
        convert( chunk.content, Nil )( factory )
    
    private def convert(
            content : String,
            current : List[ Span ]
        ) ( implicit
            factory : ElementFactory
        ) : Span = {

        if ( content.isEmpty ) return factory.toSpan( current )

        val textOnly =
            SpanMatch( content.length, None, factory.text( content ), None )

        val bestMatch = ( textOnly /: matchers ){ (current, matcher) =>
            matcher.find( content, convert( _, Nil )( factory ) ) match {
                case None => current
                case Some( nextMatch ) => {
                    if ( nextMatch.index < current.index )
                        nextMatch
                    else
                        current
                }
            }
        }
        
        val updated =
            current ::: bestMatch.before.toList ::: List( bestMatch.current )
        
        bestMatch.after match {
            case None              => factory.toSpan( updated )
            case Some( remaining ) => convert( remaining, updated )
        }
    }
}
