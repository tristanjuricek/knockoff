package com.tristanhunt.knockoff.latex

import com.tristanhunt.knockoff._
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

