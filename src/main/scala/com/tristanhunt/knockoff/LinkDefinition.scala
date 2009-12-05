package com.tristanhunt.knockoff

import scala.xml.{ Node, Group }
import scala.util.parsing.input.Position


class LinkDefinition(
  val id        : String,
  val url       : String,
  val title     : Option[ String ],
  val position  : Position
)
extends SimpleBlock {

  val span = Span.empty

  def xml : Node = Group( Nil )
  
  def markdown = {
    val sb = new StringBuilder
    sb.append("[").append( id ).append("]: ").append( url ).append(
      title match {
        case None => ""
        // Remember that "This "Title" is Valid" as a single title.
        case Some( titleValue ) => "\"" + titleValue + "\""
      }
    )
    sb.toString
  }
  
  override def toString = "LinkDefinition(" + markdown + ")"
  
  override def equals( rhs : Any ):Boolean = rhs match {
    case oth : LinkDefinition => ( oth canEqual this ) && ( this sameElements oth )
    case _ => false
  }
  
  def canEqual( p : LinkDefinition ) : Boolean = ( getClass == p.getClass )
  
  def sameElements( p : LinkDefinition ) : Boolean = {
    ( id == p.id ) &&
    ( url == p.url ) &&
    ( title == p.title ) &&
    ( position == p.position )
  }
  
  override def hashCode : Int =
    43 + id.hashCode + url.hashCode + span.hashCode + position.hashCode
}
