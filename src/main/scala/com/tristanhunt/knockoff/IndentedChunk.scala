package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

/**
  This represents a group of lines that have at least 4 spaces/1 tab preceding
  the line.
*/
case class IndentedChunk( val content : String ) extends Chunk {
  /**
    If the block before is a list, we append this to the end of that list.
    Otherwise, append it as a new code block. Two code blocks will get combined
    here (because it's common to have an empty line not be indented in many
    editors). Appending to the end of a list means that we strip out the first
    indent and reparse things.
  */
  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    if ( list.isEmpty ) {
      spans.first match {
        case text : Text => list += elementFactory.codeBlock( text, position )
      }
    } else {
      list.last match {
        case ml : MarkdownList =>
          val bs = discounter.knockoff( content )
          val updated = ( ml /: bs )( (ml, block) => ml + block )
          list.update( list.length - 1, updated )

        case cb : CodeBlock =>
          spans.first match {
            case text : Text =>
              list.update( list.length - 1,
                           elementFactory.codeBlock(
                             elementFactory.text(cb.text.content + text.content),
                             cb.position ) )

            case s : Span =>
              error( "Expected Text(code) for code block append, not " + s )
          }

        case _ =>
          spans.first match {
            case text : Text =>
              list += elementFactory.codeBlock( text, position )
            case s : Span =>
              error( "Expected Text(code) for code block addition, not " + s )
          }
      }
    }
  }
}
