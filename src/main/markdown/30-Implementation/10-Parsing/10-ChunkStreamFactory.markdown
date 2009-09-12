# `ChunkStreamFactory` #

Breaks the markdown document in to `Chunk`s, so that later recognition can function.
This means this

* Identifies the major boundaries of block elments
* Figures out the `LinkDefinition`s. Those are needed for Span recognition.

Notably, this remembers the position of each chunk in the input.
    
    // In knockoff2/ChunkStreamFactory.scala
    package knockoff2

    import scala.util.parsing.combinator.Parsers
    import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
    import scala.util.logging.Logged

    trait ChunkStreamFactory extends ChunkParsers with Logged {

        // See the PositionConverter

        def createChunkStream( str : String ) : Stream[ (Chunk, Position) ] =
            createChunkStream( new CharSequenceReader( str, 0 ) )
        
        def createChunkStream( reader : Reader[Char] )
            : Stream[ (Chunk, Position) ] = {
            
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
                    ( result, reader.pos ),
                    createChunkStream( next )
                )
            }
        }
    }

