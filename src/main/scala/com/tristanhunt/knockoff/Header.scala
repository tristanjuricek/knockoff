package com.tristanhunt.knockoff

import scala.xml.Elem
import scala.util.parsing.input.Position

class Header(
  val level     : Int,
  val span      : SpanSeq,
  val position  : Position
)
extends SimpleBlock {

  def markdown = {
    val sb = new StringBuilder
    ( 0 until level ).foreach( _ => sb.append("#") )
    sb.append(" ").append( span.toMarkdown ).append(" ")
    ( 0 until level ).foreach( _ => sb.append("#") )
    sb.toString
  }
  
  def xml = level match {
    case 1 => <h1>{ span.toXML }</h1>
    case 2 => <h2>{ span.toXML }</h2>
    case 3 => <h3>{ span.toXML }</h3>
    case 4 => <h4>{ span.toXML }</h4>
    case 5 => <h5>{ span.toXML }</h5>
    case 6 => <h6>{ span.toXML }</h6>
    case _ => <div class={ "header" + level }>{ span.toXML }</div>
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
