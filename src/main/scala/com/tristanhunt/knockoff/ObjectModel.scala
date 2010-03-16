package com.tristanhunt.knockoff

import scala.io.{ Source }
import scala.util.parsing.input.{ NoPosition, Position }

trait Span

case class Text( content : String ) extends Span
case class HTMLSpan( html : String ) extends Span
case class CodeSpan( content : String ) extends Span

case class Strong( children : Seq[Span] ) extends Span
case class Emphasis( children : Seq[Span] ) extends Span

case class Link( children : Seq[Span], url : String, title : Option[String] )
extends Span

case class IndirectLink( children : Seq[Span], definition : LinkDefinition )
extends Span

case class ImageLink( children : Seq[Span], url : String,
                      title : Option[String] )
extends Span

case class IndirectImageLink( children : Seq[Span],
                              definition : LinkDefinition )
extends Span

trait Block { def position : Position }

case class Paragraph( spans : Seq[Span], position : Position ) extends Block

case class Header( level : Int, spans : Seq[Span], position : Position )
extends Block

case class LinkDefinition( id : String, url : String, title : Option[String],
                           position : Position )
extends Block

case class Blockquote( children : Seq[Block], position : Position )
extends Block

case class CodeBlock( text : Text, position : Position ) extends Block

case class HorizontalRule( position : Position ) extends Block

case class OrderedItem( children : Seq[Block], position : Position )
extends Block

case class UnorderedItem( children : Seq[Block], position : Position )
extends Block

case class OrderedList( items : Seq[OrderedItem] ) extends Block {
  lazy val position = if ( items.isEmpty ) NoPosition else items.first.position
}

case class UnorderedList( items : Seq[UnorderedItem] ) extends Block {
  lazy val position = if ( items.isEmpty ) NoPosition else items.first.position      
}
