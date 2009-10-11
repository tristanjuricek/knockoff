package knockoff

import scala.xml.Elem
import scala.util.parsing.input.Position

class Paragraph(
  val span      : SpanSeq,
  val position  : Position
)
extends SimpleBlock {

  def markdown = span.toMarkdown

  /**
    If this paragraph only contains HTMLSpan elements, then just pass that
    information through without a paragraph marker.
  */
  def xml =
    if ( isHTML ) span.toXML else <p>{ span.toXML }</p>
  
  def isHTML : Boolean = ! span.exists( s => s match {
    case html : HTMLSpan => false
    case t:Text => ! t.content.trim.isEmpty
    case _ => true
  } )
  
  override def toString = "Paragraph(" + markdown + ")"
  
  override def equals( rhs : Any ):Boolean = rhs match {
    case oth : Paragraph => ( oth canEqual this ) && ( this sameElements oth )
    case _ => false
  }
  
  def canEqual( p : Paragraph ) : Boolean = ( getClass == p.getClass )
  
  def sameElements( p : Paragraph ) : Boolean = {
    ( span == p.span ) &&
    ( position == p.position )
  }
  
  override def hashCode : Int = span.hashCode + position.hashCode
}
