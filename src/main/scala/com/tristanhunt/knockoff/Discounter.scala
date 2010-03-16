package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position
import scala.util.parsing.input.CharSequenceReader
import scala.xml.{ Group, Node }

trait Discounter extends ChunkStreamFactory with XHTMLWriter with TextWriter {

  /** Parses and returns our best guess at the sequence of blocks. It will
      never fail, just log all suspicious things. */
  def knockoff( source : java.lang.CharSequence ) : Seq[Block] = {
      
    val chunks = createChunkStream( new CharSequenceReader( source, 0 ) )

    // These next lines are really ugly because I couldn't figure out a nice
    // way to match a tuple argument (thank you erasure!)
    val linkDefinitions = chunks.flatMap{ case ((chunk, pos)) =>
      if ( chunk.isLinkDefinition )
        List( chunk.asInstanceOf[ LinkDefinitionChunk ] )
      else Nil
    }
    
    val convert = new SpanConverter( linkDefinitions )
    
    val spanned = chunks.map { chunkAndPos =>
      ( chunkAndPos._1, convert( chunkAndPos._1 ), chunkAndPos._2 )
    }
    
    combine( spanned.toList, new ListBuffer )
  }
  
  /** Consume input and append the right thing to the output until empty. The
      Chunk itself determines the "right thing to do". All chunks only know what
      has come before itself, by peering into the output. (It shouldn't matter
      what comes next...) */
  private def combine( input : List[ (Chunk, Seq[Span], Position) ],
                       output : ListBuffer[Block] )
                     : Seq[ Block ] = {
    if ( input.isEmpty ) return output
    input.head._1.appendNewBlock( output, input.tail, input.head._2,
                                  input.head._3, this )
    combine( input.tail, output )
  }
}

