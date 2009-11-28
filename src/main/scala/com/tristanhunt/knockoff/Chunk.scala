package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

trait Chunk {
  def content : String
  
  def isLinkDefinition = false

  /** Create the Block and append to the list. */
  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq, position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
}
