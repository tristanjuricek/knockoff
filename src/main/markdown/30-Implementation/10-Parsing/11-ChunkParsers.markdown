## `ChunkParsers` ##

Mostly, this is a series of regular expressions built to find the next chunk in a
markdown document. Some expressions, like headings, will determine the "real span"
part separately from the expressions here.

    // In knockoff2/ChunkParsers.scala
    package knockoff2
    
    import scala.util.parsing.combinator.RegexParsers

    trait ChunkParsers extends RegexParsers {
        
        override def skipWhitespace = false
        
        def chunk : Parser[ Chunk ] = {
            textBlock | emptyLines
        }
        
        def emptyLines : Parser[ Chunk ] =
            rep1( emptyLine ) ^^ ( str => EmptySpace( foldedString( str ) ) )
        
        def emptyLine : Parser[ Chunk ] =
            """[\t ]*\n""".r ^^ ( str => EmptySpace( str ) )

        def textBlock : Parser[ Chunk ] =
            rep1( textLine ) ^^ ( str => TextChunk( foldedString( str ) ) )

        /** Match any line up until it ends with a newline. */
        def textLine : Parser[ Chunk ] =
            """[\t ]*\S[^\n]*\n?""".r ^^ ( str => TextChunk( str ) )
        
        // Utility Methods
        
        /** Take a series of very similar chunks and group them. */
        private def foldedString( texts : List[ Chunk ] ) : String =
            ( "" /: texts )( (current, text) => current + text.content )
    }
