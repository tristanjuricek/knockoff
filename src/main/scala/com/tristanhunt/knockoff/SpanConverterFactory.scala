package com.tristanhunt.knockoff

trait SpanConverterFactory extends HasElementFactory {
 
  def spanConverter( definitions : Seq[ LinkDefinitionChunk ] ) : Chunk => SpanSeq =
    new SpanConverter( definitions, elementFactory )
}
