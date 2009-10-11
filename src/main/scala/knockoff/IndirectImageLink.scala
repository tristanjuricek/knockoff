package knockoff

import scala.xml.Node

class IndirectImageLink(
  children    : SpanSeq,
  definition  : LinkDefinition
)
extends IndirectLink( children, definition )
with    ImageSpan {
  override def toString = "IndirectImageLink(" + markdown + ")"
  
  override def hashCode : Int = {
    41 + ( (11 /: children){
      (sum, child) => 41 + sum + 11 * child.hashCode
    } )
  }
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : IndirectImageLink =>
      ( t.canEqual( this ) ) && ( this sameElements t )
    case _ => false
  }
  
  def canEqual( s : IndirectImageLink ) : Boolean = s.getClass == getClass
}
