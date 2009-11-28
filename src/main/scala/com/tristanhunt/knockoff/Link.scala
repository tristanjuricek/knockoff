package com.tristanhunt.knockoff

import scala.util.Random
import scala.xml._

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
    <a href={ escapedOrPlainURL } title={ title.getOrElse(null) }>{ childrenXML }</a>
  
  def escapedOrPlainURL =
    if ( url startsWith "mailto:" ) Unparsed( escapedURL ) else Text( url )
  
  def escapedURL = {
    val rand = new Random
    url.map { ch =>
      rand.nextInt(2) match {
        case 0 => java.lang.String.format( "&#%d;", int2Integer( ch.toInt ) )
        case 1 => java.lang.String.format( "&#x%s;", ch.toInt.toHexString )
      }
    }.mkString("")
  }
  
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
