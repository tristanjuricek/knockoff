package com.tristanhunt.knockoff.latex

import com.tristanhunt.knockoff._
import java.io.{ StringWriter, Writer }
import scala.xml.{ Node, Text => XMLText }

trait LatexWriter extends XHTMLWriter {
  
  override def blockToXHTML : Block => Node = block => block match {
    case LatexBlock( latex, _ ) => XMLText( latex )
    case _ => super.blockToXHTML( block )
  }
  
  override def spanToXHTML : Span => Node = span => span match {
    case LatexSpan( latex ) => XMLText( latex )
    case _ => super.spanToXHTML( span )
  }
  
  def ruleWidth : String = "\\textwidth"
  def ruleHeight : String = ".3pt"
  
  def toLatex( blocks : Seq[Block] ) : String = {
    val sw = new StringWriter
    toLatex( blocks, sw )
    sw.getBuffer.toString
  }
  
  def toLatex( blocks : Seq[Block], writer : Writer ) : Unit =
    toLatex( blocks, 0 )( writer )
  
  def toLatex( blocks : Seq[Block], depth : Int )( implicit writer : Writer )
             : Unit =
    blocks.foreach { toLatex( _, depth ) }
  
  def toLatex( block : Block, depth : Int )( implicit writer : Writer )
             : Unit = {
    block match {
      case Paragraph( spans, _ ) => paragraphToLatex( spans )
      case Header( level, spans, _ ) => headerToLatex( level, spans )
      case LinkDefinition( _, _, _, _ ) => {}
      case Blockquote( children, _ ) => blockquoteToLatex( children, depth )
      case CodeBlock( text, _ ) => codeToLatex( text.content )
      case HorizontalRule( _ ) => latexRule
      case OrderedItem( children, _ ) => liToLatex( children, depth )
      case UnorderedItem( children, _ ) => liToLatex( children, depth )
      case OrderedList( items ) => olToLatex( items, depth )
      case UnorderedList( items ) => ulToLatex( items, depth )
      case LatexBlock( latex, _ ) => writer.write( latex )
      case _ => {}
    }
    if ( depth == 0 ) writer.write("\n")
  }
  
  def paragraphToLatex( spans : Seq[Span] )( implicit writer : Writer ) : Unit = {
    spans.foreach( toLatex )
    writer.write("\n")
  }
  
  /** Attempts to map the header as article sections. */
  def headerToLatex( level : Int, spans : Seq[Span] )( implicit writer : Writer )
                   : Unit = {
    level match {
      case 1 => writer.write( "\\section{" )
      case 2 => writer.write( "\\subsection{" )
      case _ => writer.write( "\\subsubsection{" )
    }
    spans.foreach( toLatex )
    writer.write("}\n")
  }
  
  def blockquoteToLatex( children : Seq[Block], depth : Int )
                       ( implicit writer : Writer )
                       : Unit = {
    writer.write( "\\begin{quote}\n" )
    toLatex( children, depth + 1 )
    writer.write( "\\end{quote}\n" )
  }
  
  def codeToLatex( text : String )( implicit writer : Writer ) : Unit = {
    writer.write( "\\begin{verbatim}\n" )
    writer.write( text )
    writer.write( "\\end{verbatim}\n" )
  }
  
  def latexRule( implicit writer : Writer ) : Unit =
    List( "\\rule{", ruleWidth, "}{", ruleHeight, "}\n" )
      .foreach( writer.write )
  
  def olToLatex( items : Seq[OrderedItem], depth : Int )
               ( implicit writer : Writer )
               : Unit = {
    writer.write( "\\begin{enumerate}\n" )
    toLatex( items, depth + 1 )
    writer.write( "\\end{enumerate}\n" )
  }
  
  def ulToLatex( items : Seq[UnorderedItem], depth : Int )
               ( implicit writer : Writer )
               : Unit = {
    writer.write( "\\begin{itemize}\n" )
    toLatex( items, depth + 1 )
    writer.write( "\\end{itemize}\n" )
  }
  
  def liToLatex( items : Seq[Block], depth : Int )( implicit writer : Writer )
               : Unit = {
    writer.write( "\\item " )
    toLatex( items, depth )
  }
  
  def toLatex( span : Span )( implicit writer : Writer ) : Unit = span match {
    case Text( content ) => textToLatex( content )
    case HTMLSpan( html ) => htmlSpanToLatex( html )
    case CodeSpan( code ) => codeSpanToLatex( code )
    case Strong( children ) => strongToLatex( children )
    case Emphasis( children ) => emphasisToLatex( children )
    case Link( children, url, title ) => linkToLatex( children, url, title )
    case IndirectLink( children, definition ) =>
      linkToLatex( children, definition.url, definition.title )
    case ImageLink( children, url, title ) => imageLinkToLatex( children, url, title )
    case IndirectImageLink( children, definition ) =>
      imageLinkToLatex( children, definition.url, definition.title )
    case LatexSpan( content ) => writer.write( content )
    case _ => {}
  }
  
  def textToLatex( content : String )( implicit writer : Writer ) : Unit = {
    writer.write( content )
  }
  
  def htmlSpanToLatex( html : String )( implicit writer : Writer ) : Unit = {
    // do nothing for now...
  }
  
  def codeSpanToLatex( code : String )( implicit writer : Writer ) : Unit = {
    writer.write( "\\verb|" )
    writer.write( code.replace("|", "\\|") )
    writer.write("| ")
  }
  
  def strongToLatex( children : Seq[Span] )( implicit writer : Writer ) : Unit = {
    writer.write( "\\textbf{" )
    children.foreach( toLatex )
    writer.write( "} " )
  }
  
  def emphasisToLatex( children : Seq[Span] )( implicit writer : Writer ): Unit ={
    writer.write( "\\textit{" )
    children.foreach( toLatex )
    writer.write( "} " )
  }
  
  def linkToLatex( children : Seq[Span], url : String, title : Option[String] )
                 ( implicit writer : Writer )
                 : Unit = {
    children.foreach( toLatex )
    writer.write( "\\footnote{" )
    if ( title.isDefined ) {
      writer.write( title.get )
      writer.write( " " )
    }
    writer.write( url )
    writer.write( "}" )
  }
  
  def imageLinkToLatex( children : Seq[Span], url : String,
                        title : Option[String] )
                      ( implicit writer : Writer )
                      : Unit = {
    writer.write( "\\begin{figure}\n" )
    writer.write( "\\includegraphics{" )
    writer.write( url )
    writer.write( "}" )
    writer.write( "\\caption{" )
    children.foreach( toLatex )
    writer.write( "}\n")
    writer.write( "\\end{figure}\n" )
  }
}
