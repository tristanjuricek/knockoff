package knockoff2

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.util.parsing.input.CharSequenceReader

trait   Discounter
extends ChunkStreamFactory
with    SpanConverterFactory
with    HasElementFactory {

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
    
    combine( spanned.toList, new ListBuffer )
  }
  
  /**
    Consume input and append the right thing to the output until empty. The
    Chunk itself determines the "right thing to do". All chunks only know what
    has come before itself, by peering into the output. (It shouldn't matter
    what comes next...)
  */
  private def combine(
    input   : List[ (Chunk, SpanSeq, Position) ],
    output  : ListBuffer[ Block ]
  ) : BlockSeq = {

    if ( input.isEmpty ) return new GroupBlock( output.toSeq )

    input.head._1.appendNewBlock(
      output,         // Adds block to the _end_
      input.head._2,  // The spanning sequence (may be ignored)
      input.head._3   // The position shoudl be passed through
    )( elementFactory, this )

    combine( input.tail, output )
  }
}
