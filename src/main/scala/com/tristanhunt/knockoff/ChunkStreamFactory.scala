package com.tristanhunt.knockoff

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
import scala.util.logging.Logged

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
