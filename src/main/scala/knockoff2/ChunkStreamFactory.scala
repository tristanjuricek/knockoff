package knockoff2

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
import scala.util.logging.Logged

trait ChunkStreamFactory extends ChunkParsers with Logged {

    def createChunkStream( str : String ) : Stream[ (Chunk, Position) ] =
        createChunkStream( new CharSequenceReader( str, 0 ) )
    
    def createChunkStream( reader : Reader[Char] )
        : Stream[ (Chunk, Position) ] = {
        
        if ( reader.atEnd ) return Stream.empty
        
        parse( chunk, reader ) match {

            case Error( msg, next ) => {
                log( msg )
                log( "next == reader : " + (next == reader) )
                createChunkStream( next )
            }
            
            case Failure( msg, next ) => {
                log( msg )
                log( "next == reader : " + (next == reader) )
                createChunkStream( next )
            }
            
            case Success( result, next ) => Stream.cons(
                ( result, reader.pos ),
                createChunkStream( next )
            )
        }
    }
}
