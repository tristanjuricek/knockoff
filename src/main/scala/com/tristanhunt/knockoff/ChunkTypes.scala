package com.tristanhunt.knockoff

import scala.collection.mutable.{ Buffer, ListBuffer }
import scala.util.parsing.input.{ Position }

trait Chunk {
  def content : String
  def isLinkDefinition = false

  /** Create the Block and append to the list. */
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter )
}

case class BlockquotedChunk( content : String ) extends Chunk {

  /** @param content The material, not parsed, but also not containing this
                     level's '>' characters. */
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    val blocks = discounter.knockoff( content )
    list += Blockquote( blocks, position )
  }
}

case class EmptySpace( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    if ( remaining.isEmpty ) return
    if ( list.isEmpty ) return
    list.last match {
      case lastCB : CodeBlock =>
        remaining.head._1 match {
          case ice : IndentedChunk =>
            list.update( list.length - 1,
                         CodeBlock( Text( lastCB.text.content + "\n" ),
                                    lastCB.position ) )
          case _ => {}
        }
      case _ => {}
    }
  }
}

case class HeaderChunk( val level : Int, val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += Header( level, spans, position )
  }
}

case object HorizontalRuleChunk extends Chunk {
  val content = "* * *\n"
  
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += HorizontalRule( position )
  }
}

case class IndentedChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    if ( list.isEmpty ) {
      spans.first match {
        case text : Text => list += CodeBlock( text, position )
      }
    } else {
      list.last match {
        case OrderedList( items ) =>
          val blocks = discounter.knockoff( content )
          val li = OrderedItem( items.last.children ++ blocks, items.last.position )
          list.update( list.length - 1, OrderedList( items.take( items.length - 1) ++ List(li) ) )

        case UnorderedList( items ) =>
          val blocks = discounter.knockoff( content )
          val li =
            UnorderedItem( items.last.children ++ blocks, items.last.position )
          list.update( list.length - 1, UnorderedList( items.take( items.length - 1) ++ List(li) ) )

        case CodeBlock( text, position ) =>
          spans.first match {
            case next : Text =>
              list.update( list.length - 1,
                           CodeBlock(Text(text.content + next.content), position) )
          }
        
        case _ =>
          spans.first match {
            case text : Text => list += CodeBlock( text, position )
          }
      }
    }
  }
}

case class LinkDefinitionChunk( val id : String, val url : String,
                                val title : Option[ String ] )
extends Chunk {

  override def isLinkDefinition = true
  
  def content : String =
    "[" + id + "]: " + url + title.map( " \"" + _ + "\"" ).getOrElse("")
  
  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += LinkDefinition( id, url, title, position )
  }
}

case class NumberedLineChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    val li = OrderedItem( List(Paragraph(spans, position)), position )
    if ( list.isEmpty ) {
      list += OrderedList( List(li) )
    } else {
      list.last match {
        case ol : OrderedList =>
          list.update( list.length - 1, OrderedList( ol.items ++ List(li) ) )
        case _ => list += OrderedList( List(li) )
      }
    }
  }
}

case class TextChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {

    val (lead, rest) = splitAtHardBreak( spans, new ListBuffer )
    list += Paragraph( lead, position )
    if ( ! rest.isEmpty )
      appendNewBlock( list, remaining, rest, position, discounter )
  }

  def splitAtHardBreak( spans : Seq[Span], cur : Buffer[Span] )
                      : (Seq[Span], Seq[Span]) = {
    if ( spans.isEmpty ) return ( cur, spans )
    spans.first match {
      case text : Text =>
        text.content.indexOf("  \n") match {
          case -1 => {}
          case idx =>
            val end = idx + "  \n".length
            val (c1, c2) = ( text.content.substring( 0, end ),
                             text.content.substring( end ) )
            cur += Text(c1)
            return ( cur, List(Text(c2)) ++ spans.drop(1) )
        }
      case _ => {}
    }
    return splitAtHardBreak( spans.drop(1), cur + spans.first )
  }
}

case class BulletLineChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    val li = UnorderedItem( List(Paragraph(spans, position)), position )
    if ( list.isEmpty ) {
      list += UnorderedList( List(li) )
    } else {
      list.last match {
        case ul : UnorderedList =>
          list.update( list.length - 1, UnorderedList( ul.items ++ List(li) ) )
        case _ => list += UnorderedList( List(li) )
      }
    }
  }
}
