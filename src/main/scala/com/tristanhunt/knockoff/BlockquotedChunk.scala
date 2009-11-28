package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

/**
  @param content The material, not parsed, but also not containing this level's
                 '>' characters.
*/
case class BlockquotedChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    val blocks = discounter.knockoff( content )
    list += elementFactory.blockquote( blocks, position )
  }
}
