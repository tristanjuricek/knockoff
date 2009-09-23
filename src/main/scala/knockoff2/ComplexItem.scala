package knockoff2

import scala.util.parsing.input.Position

abstract class ComplexItem(
  val children : BlockSeq,
  val position : Position
)
extends ComplexBlock
with    PrefixedItem {
    
  def xml = <li>{ childrenXML }</li>
  
  def markdown : String = {
    if ( children.isEmpty ) return ""

    return (
      ( itemPrefix + children.first.markdown + "  \n" ) +
      children.drop(1).map( "    " + _.markdown + "  \n" ).mkString( "" )
    )
  }
  
  override def toString = "ComplexItem(" + markdown + ")"
  
  override def hashCode : Int = position.hashCode + 47
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : ComplexItem => t.canEqual( this ) && ( this sameElements t )
    case _ => false
  }
  
  def sameElements( ci : ComplexItem ) : Boolean = {
    ( children == ci.children ) &&
    ( position == ci.position )
  }
  
  def canEqual( t : ComplexItem ) : Boolean = t.getClass == getClass
}

class   OrderedComplexItem( children : BlockSeq, position : Position )
extends ComplexItem( children, position )
with    OrderedItem

class   UnorderedComplexItem( children : BlockSeq, position : Position )
extends ComplexItem( children, position )
with    UnorderedItem
