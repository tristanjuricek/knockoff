package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

case class LinkDefinitionChunk( val id : String, val url : String,
                                val title : Option[ String ] )
extends Chunk {

  override def isLinkDefinition = true
  
  def content : String = "[" + id + "]: " + url + (
    title.map( " \"" + _ + "\"" ).getOrElse("")
  )
  
  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    list += elementFactory.linkdef( id, url, title, position )
  }
}
