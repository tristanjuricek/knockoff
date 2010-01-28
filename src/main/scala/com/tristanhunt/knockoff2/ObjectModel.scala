package com.tristanhunt.knockoff2

import java.net.{ URI }
import scala.util.parsing.input.{ NoPosition, Position }

trait Block {
  def source : String
  def pos : Position = NoPosition
}

case class BlockEnd( val source : String, override val pos : Position )
extends    Block

case class EmptyBlock( val source : String, override val pos : Position )
extends    Block

case class LinkDefinition( val uri : URI, val id : String,
                           val title : Option[String], val source : String,
                           override val pos : Position )
extends    Block

case class PlainText( val text : String, val source : String,
                      override val pos : Position )
extends    Block
