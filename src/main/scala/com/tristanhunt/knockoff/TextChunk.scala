package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

/** Mostly, a Chunk that is not empty. */
case class TextChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    list += elementFactory.para( elementFactory.toSpan( spans ), position )
  }
}
