# `Chunk` Types #

This is more of a reference to the typing of chunks.

    // In knockoff2/ChunkTypes.scala
    package knockoff2
    
    import scala.collection.mutable.ListBuffer
    import scala.util.parsing.input.Position
    
    trait Chunk {
      def content : String

      /** Create the Block and append to the list. */
      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory )
    }
    
    /** Mostly, a Chunk that is not empty. */
    case class TextChunk( val content : String ) extends Chunk {

      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        list += elementFactory.para( elementFactory.toSpan( spans ), position )
      }
    }
    
    case object HorizontalRuleChunk extends Chunk {
      val content = "* * *\n"
      
      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        list += elementFactory.hr( position )
      }
    }

    /** Note that this does not cover forced line breaks. */
    case class EmptySpace( val content : String ) extends Chunk {

      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        // This space for rent.
      }
    }

    case class BulletLineChunk( val content : String ) extends Chunk {

      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        val li = elementFactory.usi( elementFactory.toSpan(spans), position )
        if ( list.isEmpty ) {
          list += elementFactory.simpleUL( li )
        } else {
          list.last match {
            case ul : UnorderedList => {
              val appended = ul + li
              list.update( list.length - 1, appended )
            }
            case _ => list += elementFactory.simpleUL( li )
          }
        }
      }
    }
    
    case class NumberedLineChunk( val content : String ) extends Chunk {

      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        val li = elementFactory.osi( elementFactory.toSpan(spans), position )
        if ( list.isEmpty ) {
          list += elementFactory.simpleOL( li )
        } else {
          list.last match {
            case ol : OrderedList => {
              val appended = ol + li
              list.update( list.length - 1, appended )
            }
            case _ => list += elementFactory.simpleOL( li )
          }
        }
      }
    }
    
    case class HeaderChunk( val level : Int, val content : String ) extends Chunk {

      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        list += elementFactory.head( level, elementFactory.toSpan(spans), position )
      }
    }

    /**
      This represents a group of lines that have at least 4 spaces/1 tab preceding
      the line.
    */
    case class IndentedChunk( val content : String ) extends Chunk {
      
      lazy val lines = io.Source.fromString( content ).getLines.toList
      
      /**
        If the block before is a list, we append this to the end of that list.
        Otherwise, append it as a new block item.
      */
      def appendNewBlock(
        list     : ListBuffer[ Block ],
        spans    : SpanSeq,
        position : Position
      )( elementFactory : ElementFactory ) {
        
      }
    }
