package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff._
import com.tristanhunt.knockoff.latex.{ LatexWriter }
import java.io.{ Writer, StringWriter }
import scala.xml.{ Node, Unparsed }

case class InterpolatedSCAML( content : String ) extends Span

trait SCAMLDiscounter extends Discounter with StringExtras
  with SCAMLXHTMLWriter with SCAMLPlainTextWriter with SCAMLWriter {
  
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


trait SCAMLWriter { self : TextWriter =>
  
  // TODO -> theIndent and currentIndent actually shouldn't be accessible. 
  val theIndent = "  "
  
  val currentIndent = new StringBuilder
  
  def indent = currentIndent.toString
  
  def increaseIndent = currentIndent.append( theIndent )
  
  def decreaseIndent = currentIndent.delete( currentIndent.length - theIndent.length,
                                             currentIndent.length )
  
  
  /** Generates a SCAML version of the markdown document. */
  def toSCAML( blocks : Seq[Block] ) : String = {
    implicit val writer = new StringWriter
    blocksToSCAML( blocks )
    writer.toString
  }
  
  def blocksToSCAML( blocks : Seq[Block] )( implicit writer : Writer ) : Unit =
    blocks.foreach( blockToSCAML )
  
  /** Each block level element starts a new line, with indent, and increases
      the current indent level before continuing with what it contains.
      When done, it reduces the indent level. */
  def blockToSCAML( block : Block )( implicit writer : Writer ) : Unit = {
    if ( block.isInstanceOf[LinkDefinition] ) return
    writer.write( indent )
    block match {          
      case Paragraph( _, _ ) => writer.write( "%p\n" )
      case Header( level, _, _ ) =>
        writer.write( "%h" )
        writer.write( level.toString )
        writer.write( "\n" )
      case Blockquote( _, _ ) =>
        writer.write( "%blockquote\n" )
      case CodeBlock( _, _ ) => 
        writer.write( "%pre\n" )
        increaseIndent
        writer.write( indent )
        writer.write( "%code\n" )
      case HorizontalRule( _ ) =>
        writer.write( "%hr\n" )
      case OrderedItem( _, _ ) | UnorderedItem( _, _ ) =>
        writer.write( "%li\n" )
      case OrderedList( _ ) =>
        writer.write( "%ol\n" )
      case UnorderedList( _ ) =>
        writer.write( "%ul\n" )
      case _ => error( "Unknown block: " + block )
    }
    increaseIndent
    block match {
      case Paragraph( spans, _ ) => spans.foreach( spanToSCAML )
      case Header( _, spans, _ ) => spans.foreach( spanToSCAML )
      case Blockquote( children, _ ) => children.foreach( blockToSCAML )
      case CodeBlock( text, _ ) =>
        // **TODO** Prefix all lines with the indent
        val indented = text.content.split("\n")
                                   .map{ l => indent + l }
                                   .mkString("\n")
        writer.write( indented + "\n" )
        decreaseIndent
      case HorizontalRule( _ ) => {}
      case OrderedItem( children, _ ) => children.foreach( blockToSCAML )
      case UnorderedItem( children, _ ) => children.foreach( blockToSCAML )
      case OrderedList( items ) => items.foreach( blockToSCAML )
      case UnorderedList( items ) => items.foreach( blockToSCAML )
      case _ => error( "Illegal block: " + block )
    }
    decreaseIndent
  }
  
  /** Delegate to the appropriate handler to decide on indentation. */
  def spanToSCAML( span : Span )( implicit writer : Writer ) : Unit = {
    span match {
      case Text(_) | HTMLSpan(_) | CodeSpan(_) => normalSpanToSCAML( span )
      case Strong(_) | Emphasis(_) | Link(_,_,_) | IndirectLink(_,_) |
           ImageLink(_,_,_) | IndirectImageLink(_,_) => spanBlockToSCAML( span )
    }
  }
  
  /** These spans operate like the block level elements above. */
  def spanBlockToSCAML( span : Span )( implicit writer : Writer ) : Unit = {
    writer.write( indent )
    span match {
      case Strong( _ ) => writer.write( "%strong\n")
      case Emphasis( children ) => writer.write( "%em\n" )
      case Link( _, url, title ) =>
        writer.write( "%a{ :href => \"" )
        writer.write( url )
        writer.write( "\"" )
        for ( t <- title ) {
          writer.write( ", :title => \"" )
          writer.write( t.replace("\"", "\\\"").replace("\n", " ").trim )
          writer.write( "\"" )
        }
        writer.write( " }\n" )
      case IndirectLink( _, definition ) =>
        writer.write( "%a{ :href => \"" )
        writer.write( definition.url )
        writer.write( "\"" )
        for ( t <- definition.title ) {
          writer.write( ", :title => \"" )
          writer.write( t.replace("\"", "\\\"").replace("\n", " ").trim )
          writer.write( "\"" )
        }
        writer.write( " }\n" )
      case ImageLink( children, url, title ) =>
        writer.write( "%img{ :src => \"" )
        writer.write( url )
        writer.write( "\"" )
        for ( t <- title ) {
          writer.write( ", :title => \"" )
          writer.write( t.replace("\"", "\\\"").replace("\n", " ").trim )
          writer.write( "\"" )
        }
        if ( ! children.isEmpty ) {
          val stringWriter = new StringWriter
          children.foreach{ c => spanToText(c)(stringWriter) }
          val escaped = stringWriter.toString.replace( "\"", "\\\"" ).replace("\n", " ").trim
          writer.write(", :alt => \"" )
          writer.write( escaped )
          writer.write( "\"" )
        }
        writer.write( " }\n" )
      case IndirectImageLink( children, definition ) =>
        writer.write( "%img{ :src => \"" )
        writer.write( definition.url )
        writer.write( "\"" )
        for ( t <- definition.title ) {
          writer.write( ", :title => \"" )
          writer.write( t.replace("\"", "\\\"") )
          writer.write( "\"" )
        }
        if ( ! children.isEmpty ) {
          val stringWriter = new StringWriter
          children.foreach{ c => spanToText(c)(stringWriter) }
          val escaped = stringWriter.toString.replace( "\"", "\\\"" ).replace("\n", " ").trim
          writer.write(", :alt => \"" )
          writer.write( escaped )
          writer.write( "\"" )
        }
        writer.write( " }\n" )          
      case _ => error( "Illegal block span: " + span )
    }
    increaseIndent
    span match {
      case Strong( children ) => children.foreach( spanToSCAML )
      case Emphasis( children ) => children.foreach( spanToSCAML )
      case Link( children, _, _ ) => children.foreach( spanToSCAML )
      case IndirectLink( children, _ ) => children.foreach( spanToSCAML )
      case ImageLink( _, _, _ ) => {}
      case IndirectImageLink( _, _ ) => {}
      case _ => error( "Illegal block span: " + span )
    }
    decreaseIndent
  }

  /** We generally take each span and put it on it's own line. */
  def normalSpanToSCAML( span : Span )( implicit writer : Writer ) : Unit = {
    writer.write( indent )
    span match {
      case Text( content ) => writer.write( content )
      case HTMLSpan( html ) => writer.write( html )
      case CodeSpan( code ) =>
        writer.write( "%code " )
        writer.write( code.replace("\n", " ") ) // There shouldn't be newlines
                                                // in here anyway
      case _ => error( "Illegal normal span: " + span )
    }
    writer.write( "\n" )
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



