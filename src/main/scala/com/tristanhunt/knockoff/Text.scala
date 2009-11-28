package com.tristanhunt.knockoff

import scala.xml.{ Node, Text => XMLText }

class Text( val content : String ) extends SimpleSpan {

  def markdown = content

  def xml : Node =
    XMLText( unescape( content ) )
  
  val escapeableChars = List(
      "\\", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!", ">"
  )

  def unescape(source:String):String = {
      var buf:String = source
      for ((escaped, unescaped) <- escapeableChars.map(ch => ("\\" + ch, ch)))
          buf = buf.replace(escaped, unescaped)
      buf
  }

  override def toString = "Text(" + content + ")"
  
  override def hashCode : Int = content.hashCode
      
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : Text => t.canEqual( this ) && ( t.content == content )
    case _ => false
  }
      
  def canEqual( t : Text ) : Boolean = t.getClass == getClass
}
