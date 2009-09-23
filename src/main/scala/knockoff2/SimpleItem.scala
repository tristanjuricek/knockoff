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
  val span     : Span,
  val position : Position   
)
extends SimpleBlock
with    PrefixedItem {
    
  def xml = <li>{ span.xml }</li>
  
  def markdown = itemPrefix + span.markdown
  
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

class   OrderedSimpleItem( span : Span, position : Position )
extends SimpleItem( span, position )
with    OrderedItem

class   UnorderedSimpleItem( span : Span, position : Position )
extends SimpleItem( span, position )
with    UnorderedItem
