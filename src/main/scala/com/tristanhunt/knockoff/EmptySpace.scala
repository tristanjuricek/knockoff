package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

/** Note that this does not cover forced line breaks. */
case class EmptySpace( val content : String ) extends Chunk {

  /**
    Empty space only matters in cases where the lines are indented, which is a
    way of dealing with editors that like to do things like strip out whitespace
    at the end of a line.
  */
  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    if ( remaining.isEmpty ) return
    if ( list.isEmpty ) return
    list.last match {
      case lastCB : CodeBlock => remaining.first._1 match {
        case ice : IndentedChunk =>
          list.update( list.length - 1,
                       elementFactory.codeBlock(
                         elementFactory.text( lastCB.text.content + "\n" ),
                         lastCB.position ) )
        case _ => {}
      }
      case _ => {}
    }
  }
}
