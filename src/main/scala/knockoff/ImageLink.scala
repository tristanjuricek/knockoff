package knockoff

import scala.xml.Node

class ImageLink(
  children  : SpanSeq,
  url       : String,
  title     : Option[ String ]
)
extends Link( children, url, title )
with    ImageSpan {
  override def toString = "ImageLink(" + markdown + ")"
  
  override def hashCode : Int =
    37 + ( (13 /: children)( (sum, child) => 37 + sum + 13 * child.hashCode ) )
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : ImageLink => t.canEqual( this ) && ( this sameElements t )
    case _ => false
  }
  
  def canEqual( s : ImageLink ) : Boolean = s.getClass == getClass
}
