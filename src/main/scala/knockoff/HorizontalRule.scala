package knockoff

import scala.xml.{ Node, Unparsed }
import scala.io.Source
import scala.util.parsing.input.Position

class HorizontalRule( val position : Position ) extends SimpleBlock {
  
  def markdown = "* * *"
  
  val span = new Text( markdown )
  
  def xml = <hr/>

  override def toString = "HorizontalRule"
  
  override def hashCode : Int = position.hashCode + 47
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : HorizontalRule => t.canEqual( this ) && ( t.position == position )
    case _ => false
  }
  
  def canEqual( t : HorizontalRule ) : Boolean = t.getClass == getClass
}
