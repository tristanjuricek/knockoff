package knockoff2

import scala.util.parsing.input.Position

trait PrefixedItem {
  def itemPrefix : String
}

trait OrderedItem extends PrefixedItem {
  def itemPrefix = "* "
}

trait UnorderedItem extends PrefixedItem {
  def itemPrefix = "1. "
}

abstract class SimpleItem (
  val span     : SpanSeq,
  val position : Position   
)
extends SimpleBlock
with    PrefixedItem {
    
  def xml = <li>{ span.toXML }</li>
  
  def markdown = itemPrefix + span.toMarkdown
  
  override def toString = "SimpleItem(" + markdown + ")"
  
  override def hashCode : Int = position.hashCode + 47
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : SimpleItem => t.canEqual( this ) && ( this sameElements t )
    case _ => false
  }
  
  def sameElements( si : SimpleItem ) : Boolean = {
    ( span == si.span ) &&
    ( position == si.position )
  }
  
  def canEqual( t : SimpleItem ) : Boolean = t.getClass == getClass
}

class   OrderedSimpleItem( span : SpanSeq, position : Position )
extends SimpleItem( span, position )
with    OrderedItem

class   UnorderedSimpleItem( span : SpanSeq, position : Position )
extends SimpleItem( span, position )
with    UnorderedItem
