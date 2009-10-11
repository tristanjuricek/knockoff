package knockoff

import scala.xml.{ Node, Group }
import scala.util.parsing.input.{ NoPosition, Position }

class   GroupBlock( val children : Seq[ Block ] )
extends ComplexBlock {
 
  val position : Position = children.firstOption match {
    case None => NoPosition
    case Some( child ) => child.position
  }
     
  def markdown = childrenMarkdown
  
  def xml : Node = Group( children.map( _.xml ) )

  override def toString = "GroupBlock(" + markdown + ")"
}
