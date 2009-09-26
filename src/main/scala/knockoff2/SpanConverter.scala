package knockoff2

class SpanConverter(
  val definitions    : Seq[ LinkDefinition ],
  val elementFactory : ElementFactory
)
extends Function1[ Chunk, SpanSeq ]
with    EqualDelimiterMatcher
with    CodeMatchers
with    EmphasisMatchers
with    StrongMatchers
with    HTMLMatchers
with    LinkMatcher
with    StringExtras {
     
  /**
    The SpanConverter is a map method from Chunk to SpanSeq
  */
  def apply( chunk : Chunk ) : SpanSeq = convert( chunk.content, Nil )
  
  /**
    Tail-recursive method halts when the content argument is empty.
  */
  protected def convert( content : String, current : List[ Span ] ) : Span = {

    if ( content.isEmpty ) return elementFactory.toSpan( current )

    val textOnly =
      SpanMatch( content.length, None, elementFactory.text( content ), None )

    val bestMatch = ( textOnly /: matchers ){ (current, findMatch) =>
      findMatch( content ) match {
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
      case None              => elementFactory.toSpan( updated )
      case Some( remaining ) => convert( remaining, updated )
    }
  }
  
  def matchers : List[ String => Option[SpanMatch] ] = List(
    matchDoubleCodes,
    matchSingleCodes,
    matchEntity,
    matchHTMLSpan,
    matchUnderscoreStrong,
    matchAsterixStrong,
    matchUnderscoreEmphasis,
    matchAsterixEmphasis
  )
}
