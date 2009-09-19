package knockoff2

trait SpanConverterFactory extends ElementFactory {
 
    def spanConverter( definitions : Seq[ LinkDefinition ] ) : Chunk => SpanSeq =
        new SpanConverter( definitions, matchers, this )
        
    def matchers : Seq[ SpanMatcher ] = List(
        DoubleCodeMatcher,
        SingleCodeMatcher,
        InlineHTMLMatcher,
        UnderscoreStrongMatcher,
        AsterixStrongMatcher,
        UnderscoreEmphasisMatcher,
        AsterixEmphasisMatcher
    )
}
