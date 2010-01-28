package com.tristanhunt.knockoff2

class MarkdownString( wrapped : String ) {
 
  /** Safely slices off the starting and leading characters of the string. */
  def trimEnds : String = {
    val start = 1
    val end = wrapped.length - 1
    if ( end < start ) wrapped else wrapped.slice( start, end )
  }
  
  def unescape : String = MarkdownString.unescape( wrapped )
}

object MarkdownString {
 
  implicit def MakeMarkdownString( wrapped : String ) =
    new MarkdownString( wrapped )
  
  val escapeableChars = "\\`*_{}[]()#+-.!>\'\""

  def unescape( source : String ) : String = {
    var buf : String = source
    for ( (escaped, unescaped) <-
            escapeableChars.map( ch => ("\\" + ch, ch.toString) ) )
      buf = buf.replace( escaped, unescaped )
    buf
  }
}
