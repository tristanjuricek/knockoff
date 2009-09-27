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
    Recursively combine the next element of the input into the output sequence.
    Because some elements (lists, code blocks separated uncleanly) affect the
    output of nearby elements, this can alter how the next element is converted.
    
    @param input The recognized element being operated on
    @param output The output sequence built in order.
  */
  private def combine(
    input   : List[ (Chunk, SpanSeq, Position) ],
    output  : ListBuffer[ Block ]
  ) : BlockSeq = {
    if ( input.isEmpty ) return new GroupBlock( output.toSeq )

    val factory = elementFactory
    import factory._

    input.firstOption.foreach{ case ((chunk, spans, position)) =>
      chunk match {
        case TextChunk(_)  => output += para( toSpan(spans), position )
        case EmptySpace(_) => {}
      }
    }
    
    combine( input.tail, output )
  }
}
