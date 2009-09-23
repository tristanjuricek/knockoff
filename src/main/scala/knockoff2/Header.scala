package knockoff2

import scala.xml.Elem
import scala.util.parsing.input.Position

class Header(
  val level     : Int,
  val span      : Span,
  val position  : Position
)
extends SimpleBlock {

  def markdown = {
    val sb = new StringBuilder
    ( 0 until level ).foreach( _ => sb.append("#") )
    sb.append(" ").append( span.markdown ).append(" ")
    ( 0 until level ).foreach( _ => sb.append("#") )
    sb.toString
  }
  
  def xml = level match {
    case 1 => <h1>{ span.xml }</h1>
    case 2 => <h2>{ span.xml }</h2>
    case 3 => <h3>{ span.xml }</h3>
    case 4 => <h4>{ span.xml }</h4>
    case 5 => <h5>{ span.xml }</h5>
    case 6 => <h6>{ span.xml }</h6>
    case _ => <div class={ "header" + level }>{ span.xml }</div>
  }

  override def toString = "Header(" + markdown + ")"
  
  override def equals( rhs : Any ):Boolean = rhs match {
    case oth : Header => ( oth canEqual this ) && ( this sameElements oth )
    case _ => false
  }
  
  def canEqual( p : Header ) : Boolean = ( getClass == p.getClass )
  
  def sameElements( p : Header ) : Boolean = {
    ( level == p.level ) &&
    ( span == p.span ) &&
    ( position == p.position )
  }
  
  override def hashCode : Int =
    43 + level + span.hashCode + position.hashCode
}
