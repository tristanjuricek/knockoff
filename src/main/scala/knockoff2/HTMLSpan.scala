package knockoff2

import scala.xml.{ Node, Unparsed }

class HTMLSpan( val content : String ) extends SimpleSpan {

  def markdown = content

  def xml : Node = Unparsed( content )

  override def toString = "HTMLSpan(" + content + ")"
  
  override def hashCode : Int = content.hashCode
      
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : HTMLSpan => t.canEqual( this ) && ( t.content == content )
    case _ => false
  }
      
  def canEqual( t : HTMLSpan ) : Boolean = t.getClass == getClass
}
