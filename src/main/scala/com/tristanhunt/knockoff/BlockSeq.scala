package com.tristanhunt.knockoff

import scala.xml.{ Node, Elem, Group }

trait BlockSeq extends Seq[ Block ] {
  
  def theSeq : Seq[ Block ]
  
  override def length : Int = theSeq.length
  
  override def elements = theSeq.elements
  
  override def apply( ii : Int ) = theSeq(ii)

  def toXML : Node = Group( theSeq.map( _.xml ) )

  /**
   * Returns a BlockSeq that contains only that particular block type.
   */
  def filterType [ T <: Block ] ( blockType : BlockType[T] ) : BlockSeq = {
    BlockSeq.fromSeq( filter( blockType.wrappedClass.isInstance( _ ) ) )
  }
  
  
  /** Shorthand for the filter method. */
  def filterType ( query : Block => Boolean ) : BlockSeq =
    BlockSeq.fromSeq( filter( query ) )
}

object BlockSeq {
    
  def fromSeq( seq : Seq[ Block ] ) = new BlockSeq {
    override def theSeq = seq
  }
}
