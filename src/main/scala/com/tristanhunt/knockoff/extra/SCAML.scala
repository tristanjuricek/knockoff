package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff._
import com.tristanhunt.knockoff.latex.{ LatexWriter }
import java.io.Writer
import scala.xml.{ Node, Unparsed }

case class InterpolatedSCAML( content : String ) extends Span

trait SCAMLDiscounter extends Discounter with StringExtras
  with SCAMLXHTMLWriter with SCAMLPlainTextWriter {
  
  override def knockoff( source : CharSequence ) : Seq[Block] =
    super.knockoff( source ).map( convertSCAMLBlock )
  
  private def convertSCAMLBlock( block : Block ) : Block = block match {
    case Paragraph( spans, pos ) =>
      Paragraph( spans.flatMap( convertSCAMLSpan ), pos )
    case Blockquote( children, pos ) =>
      Blockquote( children.map( convertSCAMLBlock ), pos )
    case OrderedList( items ) =>
      OrderedList( items.map( li => OrderedItem( li.children.map(convertSCAMLBlock),
                                                 li.position ) ) )
    case UnorderedList( items ) =>
      UnorderedList( items.map( li => UnorderedItem( li.children.map(convertSCAMLBlock),
                                                     li.position ) ) )
    case _ => block
  }
  
  private def convertSCAMLSpan( span : Span ) : Seq[Span] = span match {
    case text : Text => splitSCAML( text )
    case _ => List(span)
  }
  
  private def splitSCAML( text : Text ) : Seq[Span] = {
    val start = text.content.indexOf("#{")
    if ( -1 < start ) {
      text.content.findBalanced( '{', '}', start + 1 ) match {
        case Some( end ) =>
          val list = List( Text( text.content.substring( 0, start ) ),
                InterpolatedSCAML( text.content.substring( start, end + 1 ) ) )
          if ( end + 1 < text.content.length )
            return list ++ splitSCAML( Text(text.content.substring( end + 1 )) )
          else
            return list
        case None => return List( text )
      }
    } else {
      return List( text )
    }
  }
}


trait SCAMLXHTMLWriter extends XHTMLWriter {
  
  override def spanToXHTML : Span => Node = span => {
    span match {
      case InterpolatedSCAML( content ) => Unparsed( content )
      case _ => super.spanToXHTML( span )
    }
  }
}

trait SCAMLLatexWriter extends LatexWriter {
  
  override def toLatex( span : Span )( implicit writer : Writer ) : Unit =
    span match {
      case InterpolatedSCAML( content ) => writer.write( content )
      case _ => super.toLatex( span )
    }
}

trait SCAMLPlainTextWriter extends TextWriter {
 
  override def spanToText( span : Span )( implicit writer : Writer ) : Unit = {
    span match {
      case InterpolatedSCAML( content ) => writer.write( content + " " )
      case _ => super.spanToText( span )
    }
  }
}



