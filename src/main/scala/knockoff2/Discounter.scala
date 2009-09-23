package knockoff2

import scala.util.parsing.input.Position
import scala.util.parsing.input.CharSequenceReader

trait   Discounter
extends ChunkStreamFactory
with    SpanConverterFactory {

  /**
    Parses and returns our best guess at the sequence of blocks. It will
    never fail, just log all suspicious things.
   */
  def knockoff( source : java.lang.CharSequence ) : BlockSeq = {
      
    val chunks = createChunkStream( new CharSequenceReader( source, 0 ) )

    val linkDefinitions = chunks.flatMap( chunk => chunk match {
      case ld : LinkDefinition => List( ld )
      case _ => Nil
    } )
    
    val convert = spanConverter( linkDefinitions )
    
    val spanned = chunks.map { chunkAndPos =>
      ( chunkAndPos._1, convert( chunkAndPos._1 ), chunkAndPos._2 )
    }
    
    recombine( spanned )
  }
  
  /**
   * Part of recombination is the fancy bit.
   */
  def recombine( spannedChunks : Seq[(Chunk, SpanSeq, Position)] ) :BlockSeq = {
    error( "not implemented" )
  }
}
