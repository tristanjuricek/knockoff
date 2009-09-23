package knockoff2

import scala.xml.{ Node, Unparsed }
import scala.util.parsing.input.Position

class   HTMLBlock( val html : String, val position : Position )
extends SimpleBlock {
    
  val span = new HTMLSpan( html )
  
  def xml : Node = Unparsed( html )
  
  def markdown = html
  
  override def toString = "HTMLBlock(" + html + ")"
  
  override def hashCode : Int = html.hashCode
      
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : HTMLBlock => t.canEqual( this ) && ( this sameElements t )
    case _ => false
  }
  
  def sameElements( h : HTMLBlock ) : Boolean = {
    ( h.html == html ) &&
    ( h.position == position )
  }
  
  def canEqual( t : HTMLBlock ) : Boolean = t.getClass == getClass
}
