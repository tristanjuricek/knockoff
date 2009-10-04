package knockoff2

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

trait Chunk {
  def content : String
  
  def isLinkDefinition = false

  /** Create the Block and append to the list. */
  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter )
}

/** Mostly, a Chunk that is not empty. */
case class TextChunk( val content : String ) extends Chunk {

  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    list += elementFactory.para( elementFactory.toSpan( spans ), position )
  }
}

case object HorizontalRuleChunk extends Chunk {
  val content = "* * *\n"
  
  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    list += elementFactory.hr( position )
  }
}

/** Note that this does not cover forced line breaks. */
case class EmptySpace( val content : String ) extends Chunk {

  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    // This space for rent.
  }
}

case class BulletLineChunk( val content : String ) extends Chunk {

  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    val li = elementFactory.uli(
      elementFactory.para(spans, position),
      position
    )
    if ( list.isEmpty ) {
      list += elementFactory.ulist( li )
    } else {
      list.last match {
        case ul : UnorderedList => {
          val appended = ul + li
          list.update( list.length - 1, appended )
        }
        case _ => list += elementFactory.ulist( li )
      }
    }
  }
}

case class NumberedLineChunk( val content : String ) extends Chunk {

  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    val li = elementFactory.oli(
      elementFactory.para(spans, position),
      position
    )
    if ( list.isEmpty ) {
      list += elementFactory.olist( li )
    } else {
      list.last match {
        case ol : OrderedList => {
          val appended = ol + li
          list.update( list.length - 1, appended )
        }
        case _ => list += elementFactory.olist( li )
      }
    }
  }
}

case class HeaderChunk( val level : Int, val content : String ) extends Chunk {

  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    list += elementFactory.head( level, elementFactory.toSpan(spans), position )
  }
}

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
  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    list.last match {
      case ml : MarkdownList => {
        val bs = discounter.knockoff( content )
        val updated = ( ml /: bs )( (ml, block) => ml + block )
        list.update( list.length - 1, updated )
      }
      case _ => {
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

/**
  Represents a single level of blockquoted material.

  @param content The material, not parsed, but also not containing this level's
                 '>' characters.
*/
case class BlockquotedChunk( val content : String ) extends Chunk {
  /**
    We parse the content as a separate document, which is then appended to the
    list as a Blockquote.
  */
  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    val blocks = discounter.knockoff( content )
    list += elementFactory.blockquote( blocks, position )
  }
}

case class LinkDefinitionChunk(
  val id    : String,
  val url   : String,
  val title : Option[ String ]
) extends Chunk {

  override def isLinkDefinition = true
  
  def content : String = "[" + id + "]: " + url + (
    title.map( " \"" + _ + "\"" ).getOrElse("")
  )
  
  def appendNewBlock(
    list     : ListBuffer[ Block ],
    spans    : SpanSeq,
    position : Position
  )( elementFactory : ElementFactory, discounter : Discounter ) {
    list += elementFactory.linkdef( id, url, title, position )
  }
}
