package knockoff2

trait SpanConverterFactory extends HasElementFactory {
 
  def spanConverter( definitions : Seq[ LinkDefinition ] ) : Chunk => SpanSeq =
    new SpanConverter( definitions, elementFactory )
}
