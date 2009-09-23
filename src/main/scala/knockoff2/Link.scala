package knockoff2

import scala.xml.Node

class Link(
  val children  : SpanSeq,
  val url       : String,
  val title     : Option[ String ]
)
extends ComplexSpan {
    
  def markdown = {
    "[" + childrenMarkdown + "](" +
    url + {
      title match {
        case Some( titleString ) => " \"" + titleString + "\""
        case None => ""
      }
    } + ")"
  }
  
  def xml : Node =
    <a href={ url } title={ title.getOrElse(null) }>{ childrenXML }</a>
  
  override def toString = "Link(" + markdown + ")"
  
  override def hashCode : Int = {
    ( 43 + ( (3 /: children)( (sum, child) => 43 + sum + 3 * child.hashCode ) ) ) +
    ( 43 + url.hashCode ) +
    ( 43 + title.hashCode )
  }
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : Link => ( t.canEqual( this ) ) && ( this sameElements t )
    case _ => false
  }
  
  def canEqual( s : Link ) : Boolean = s.getClass == getClass
  
  def sameElements( l : Link ) : Boolean = {
    ( l.children sameElements children ) &&
    ( url == l.url ) &&
    ( title == l.title )
  }
}
