package knockoff2

import scala.io.Source
import scala.util.parsing.input.{ NoPosition, Position }

/**
 * @param ordered Alters the output, mostly.
 */
abstract class MarkdownList(
  val items : Seq[ ListItem ]
) extends ComplexBlock {
  
  val children = items
  
  val position = children.firstOption match {
    case None => NoPosition
    case Some( child ) => child.position
  }
  
  def markdown = childrenMarkdown
  
  /**
    Create a new list with the block appended to the last item.
  */
  def + ( block : Block ) : MarkdownList 
  
  override def toString = "MarkdownList(" + markdown + ")"
  
  override def hashCode : Int = {
    ( 13 /: children )( (total, child) => total + 51 + 3 * child.hashCode ) +
    position.hashCode + 47
  }
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : MarkdownList => t.canEqual( this ) && ( t sameElements this )
    case _ => false
  }
  
  def sameElements( ml : MarkdownList ) : Boolean = {
    ( children sameElements ml.children )
  }
  
  def canEqual( t : MarkdownList ) : Boolean = t.getClass == getClass
}

class OrderedList( items : Seq[ ListItem ] )
extends MarkdownList( items ) {
 
  def xml = <ol>{ childrenXML }</ol>
  
  def + ( item : OrderedItem ) : OrderedList =
    new OrderedList( children ++ Seq(item) )

  def + ( block : Block ) : MarkdownList = new OrderedList(
    children.take( children.length - 1 ) ++ Seq(children.last + block)
  )
}

class UnorderedList( items : Seq[ ListItem ] )
extends MarkdownList( items ) {
 
  def xml = <ul>{ childrenXML }</ul>
  
  def + ( item : UnorderedItem ) : UnorderedList =
    new UnorderedList( children ++ Seq(item) )

  def + ( block : Block ) : MarkdownList = new UnorderedList(
    children.take( children.length - 1 ) ++ Seq(children.last + block)
  )
}
