package knockoff2

import scala.io.Source
import scala.util.parsing.input.{ NoPosition, Position }

/**
 * @param ordered Alters the output, mostly.
 */
abstract class MarkdownList(
  val children : BlockSeq
) extends ComplexBlock {
    
  val position = children.firstOption match {
    case None => NoPosition
    case Some( child ) => child.position
  }
  
  def markdown = childrenMarkdown
  
  override def toString = "MarkdownList(" + markdown + ")"
  
  override def hashCode : Int = position.hashCode + 47
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : MarkdownList => t.canEqual( this ) && ( t sameElements this )
    case _ => false
  }
  
  def sameElements( ml : MarkdownList ) : Boolean = {
    ( children sameElements ml.children )
  }
  
  def canEqual( t : MarkdownList ) : Boolean = t.getClass == getClass
}

class OrderedList( children : BlockSeq )
extends MarkdownList( children ) {
 
  def xml = <ol>{ childrenXML }</ol>
  
  def + ( item : OrderedSimpleItem ) : OrderedList =
    new OrderedList( new GroupBlock( children ++ Seq( item ) ) )
  
  def + ( item : OrderedComplexItem ) : OrderedList =
    new OrderedList( new GroupBlock( children ++ Seq( item ) ) )
}

class UnorderedList( children : BlockSeq )
extends MarkdownList( children ) {
 
  def xml = <ul>{ childrenXML }</ul>
  
  def + ( item : UnorderedSimpleItem ) : UnorderedList =
    new UnorderedList( new GroupBlock( children ++ Seq( item ) ) )
  
  def + ( item : UnorderedComplexItem ) : UnorderedList =
    new UnorderedList( new GroupBlock( children ++ Seq( item ) ) )
}
