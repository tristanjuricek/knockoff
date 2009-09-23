package knockoff2

import scala.xml.Node

class Emphasis( val children : SpanSeq ) extends ComplexSpan {

  def markdown = "_" + childrenMarkdown + "_"

  def xml : Node = <em>{ childrenXML }</em>

  override def toString = "Emphasis(" + markdown + ")"
  
  override def hashCode : Int =
    43 + ( (3 /: children)( (sum, child) => 43 + sum + 3 * child.hashCode ) )
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : Emphasis => t.canEqual( this ) && ( t.children sameElements children )
    case _ => false
  }
  
  def canEqual( s : Emphasis ) : Boolean = s.getClass == getClass
}
