package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff._
import scala.util.parsing.input.{ Position }

case class LatexSpan( latex : String ) extends Span

case class LatexBlock( latex : String, position : Position ) extends Block

import java.lang.{ CharSequence }
import scala.collection.mutable.{ Buffer, ListBuffer }

trait LatexDiscounter extends Discounter {
  
  override def knockoff( source : CharSequence ) : Seq[Block] =
    super.knockoff( source ).map( toLatex )

  def toLatex( block : Block ) : Block = block match {
    case Paragraph( spans, pos ) =>
      Paragraph( spans.flatMap( toLatex ), pos )
    case CodeBlock( text, pos ) =>
      if ( isLatex( text.content ) ) LatexBlock( text.content, pos ) else block
    case Blockquote( children, pos ) =>
      Blockquote( children.map( toLatex ), pos )
    case OrderedList( items ) =>
      OrderedList( items.map{ li => OrderedItem( li.children.map( toLatex ),
                                                 li.position ) } )
    case UnorderedList( items ) =>
      UnorderedList( items.map{ li => UnorderedItem( li.children.map( toLatex ),
                                                     li.position ) } )
    case _ => block
  }
  
  def toLatex( span : Span ) : Seq[Span] = span match {
    case text : Text => splitLatex( text )
    case _ => List(span)
  }
  
  /** Find the next two un-escaped LaTeX characters. Unescape \$ sequences in
      text. */
  private def splitLatex( text : Text ) : Seq[Span] = {
    val idx = text.content.indexOf('$')
    if ( idx > 0 && text.content(idx - 1) != '\\' )
      return splitLatex( text.content, 0, new ListBuffer )
    return List(text)
  }
  
  private def splitLatex( src : String, from : Int, cur : Buffer[Span] )
                        : Seq[Span] = {
    var to = from + 1        
    while ( to != -1 && src( to - 1) == '\\' )
      to = src.indexOf( '$', to + 1 )
    
    if ( to == -1 )
      return ( cur + Text( src.substring( from ) ) )
    
    cur += LatexSpan( src.substring( from, to ) )
    
    val next = src.indexOf( '$', to + 1 )
    if ( next != -1 )
      splitLatex( src, next, cur + Text( src.substring( to, next ) ) )
    else
      cur + Text( src.substring( to ) )
  }
  
  // If first real line begins with \begin and last real line begins with \end,
  // it's latex.
  def isLatex( code : String ) : Boolean = {
    val lines = code.split("\n").filter( ! _.trim.isEmpty ).toList
    return ( 2 <= lines.length &&
             lines.head.trim.startsWith("\\begin") &&
             lines.last.trim.startsWith("\\end") )
  }
}

import uk.ac.ed.ph.snuggletex.{ SnuggleEngine, SnuggleInput }

import scala.xml.{ Node, Text => XMLText, Unparsed }

trait LatexXHTMLWriter extends XHTMLWriter {

  override def blockToXHTML : Block => Node = block => block match {
    case LatexBlock( tex, _ ) => toMathML( tex )
    case _ => super.blockToXHTML( block )
  }
  
  override def spanToXHTML : Span => Node = span => span match {
    case LatexSpan( tex ) => toMathML( tex )
    case _ => super.spanToXHTML( span )
  }
  
  def toMathML( tex : String ) : Node = {
    val engine = new SnuggleEngine
    val session = engine.createSession
    val input = new SnuggleInput(tex)
    session.parseInput(input)
    return Unparsed( session.buildXMLString )
  }
}

import java.io.{ StringWriter, Writer }
    
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
