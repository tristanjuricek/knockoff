package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

case object HorizontalRuleChunk extends Chunk {
  val content = "* * *\n"
  
  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    list += elementFactory.hr( position )
  }
}
