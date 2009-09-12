`MarkdownLexical`
=================

This recognizes critical pieces of data within the markdown, with interest in
providing:

* Input in to the `SpanParser`
* Enough data so that `Span`s can be glued together into `Block`s.

Lots of dirty tricks are in here, hidden in the crevices.

Lexemes:

* Breaks - ExtraLines, Explicit, Implicit
* ListItems - Numbered, Ordered
* IndentedBlock
* QuotedBlock
* Rule
* Headers - Types
* ParagraphBlock

Hm. How do I do this?

1. Multi-pass scanning. Lots of things can be picked out quickly. Divide and conquer
   it down.
   
2. A simpler parser combinator

The parser combinator approach is better for structured things. Like spanning
elements. Maybe, but that even requires whitespace on occasion. We'll see.

How does it do it's thing:

Try to match a result that parses a chunk at time? This would be a "stream" interface.
Is this possible?

Scan and go to the next newline (get lines?)

How is just the next sequence matched?


It looks like CharSequenceReader is perfect, and details what I want exactly.

The RegexParsers method could be use to combine into a Stream interface.

    
    // In knockoff2/ChunkStream.scala
    package knockoff2

    import scala.util.parsing.combinator.Parsers
    import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
    import scala.util.logging.Logged

    trait ChunkStreamFactory extends ChunkParsers with Logged {
        
        
        implicit def PositionConverter( p : Position ) = new PositionConverter( p )
        
        class PositionConverter( val p : Position ) {
            def asLocation = Location(
                p.line, 0, 0, io.Source.fromString( "" )
            )
        }
        
        def createChunkStream( str : String ) : Stream[ (Chunk, Location) ] =
            createChunkStream( new CharSequenceReader( str, 0 ) )
        
        def createChunkStream( reader : Reader[Char] )
            : Stream[ (Chunk, Location) ] = {
            
            if ( reader.atEnd ) return Stream.empty
            
            parse( chunk, reader ) match {

                case Error( msg, next ) => {
                    log( msg )
                    createChunkStream( next )
                }
                
                case Failure( msg, next ) => {
                    log( msg )
                    createChunkStream( next )
                }
                
                case Success( result, next ) => Stream.cons(
                    ( result, reader.pos.asLocation ),
                    createChunkStream( next )
                )
            }
        }
        

    }

## `Chunk` ##

    // In knockoff2/Chunk.scala
    package knockoff2
    
    trait Chunk {
        def content : String
    }
    
    trait Located {
        def loc : Location
    }
    
    case class TextChunk( val content : String ) extends Chunk
    
    case class EmptySpace( val content : String ) extends Chunk

## `ChunkParsers` ##

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
        
        private def foldedString( texts : List[ Chunk ] ) : String =
            ( "" /: texts )( (current, text) => current + text.content )
    }
