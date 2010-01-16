package com.tristanhunt.knockoff

import scala.xml.Node
import scala.util.parsing.input.Position

trait Block extends BlockSeq {
  
  /**
    The actual content of each block.
   */
  val span     : SpanSeq
  
  /**
    A markdown representation of this block - it may not equal the original
    source.
   */
  def markdown : String
  
  /**
    An HTML rendering of the Block element.
   */
  def xml      : Node
  
  /**
    The original source position used to make up this block.
   */
  val position : Position
}
