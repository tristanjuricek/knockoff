package knockoff

import scala.xml.Node

class Strong( val children : Seq[ Span ] ) extends ComplexSpan {
  
  def markdown = "**" + childrenMarkdown + "**"
  
  def xml : Node = <strong>{ childrenXML }</strong>
  
  override def toString = "Strong(" + markdown + ")"
  
  override def hashCode : Int =
    41 + ( (3 /: children)( (sum, child) => 41 + sum + 3 * child.hashCode ) )
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : Strong => t.canEqual( this ) && ( t.children sameElements children )
    case _ => false
  }
  
  def canEqual( s : Strong ) : Boolean = s.getClass == getClass
}
