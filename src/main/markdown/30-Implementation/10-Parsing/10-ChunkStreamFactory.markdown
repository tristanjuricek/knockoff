# `ChunkStreamFactory` #

Breaks the markdown document in to `Chunk`s, so that later recognition can function.
This means this

* Identifies the major boundaries of block elments
* Figures out the `LinkDefinition`s. Those are needed for Span recognition.

When we run into something we can't parse, there's a simple rule; go on. If I detect
that there will be more and more problems, well. Hm.

Notably, this remembers the position of each chunk in the input.

    // In knockoff2/ChunkStreamFactory.scala
    // See the ChunkStreamFactory package and imports
    
    trait ChunkStreamFactory extends Logged {

      val chunkParser = new ChunkParser

      def createChunkStream( str : String ) : Stream[(Chunk, Position)] =
        createChunkStream( new CharSequenceReader( str, 0 ) )
      
      def createChunkStream( reader : Reader[Char] ) : Stream[(Chunk, Position)] = {
          
        if ( reader.atEnd ) return Stream.empty
        
        chunkParser.parse( chunkParser.chunk, reader ) match {

          case chunkParser.Error( msg, next ) => {
            log( msg )
            log( "next == reader : " + (next == reader) )
            createChunkStream( next )
          }
          
          case chunkParser.Failure( msg, next ) => {
            log( msg )
            log( "next == reader : " + (next == reader) )
            createChunkStream( next )
          }
          
          case chunkParser.Success( result, next ) => Stream.cons(
            ( result, reader.pos ),
            createChunkStream( next )
          )
        }
      }
    }

#### Package And Imports

    // The ChunkStreamFactory package and imports
    package knockoff2

    import scala.util.parsing.combinator.Parsers
    import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
    import scala.util.logging.Logged
