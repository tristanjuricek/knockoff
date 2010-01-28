package com.tristanhunt.knockoff2

import scala.xml.{ Node, NodeBuffer, Text => XMLText }

trait XHTMLWriter {
 
  def writeXHTML( blocks : Seq[Block] ) : NodeBuffer =
    writeXHTML( blocks, new NodeBuffer )
  
  def writeXHTML( blocks : Seq[Block], nodes : NodeBuffer ) : NodeBuffer = {
    if ( blocks.isEmpty ) return nodes
    blocks.head match {
      case PlainText( text, _, _ ) =>
        writeXHTML( blocks.tail, nodes &+ <p>{ text }</p> )
      case _ =>
        writeXHTML( blocks.tail, nodes )
    }
  }
}
