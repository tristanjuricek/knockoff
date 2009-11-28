package com.tristanhunt.knockoff

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

case class BulletLineChunk( val content : String ) extends Chunk {

  def appendNewBlock( list : ListBuffer[ Block ],
                      remaining : List[ (Chunk, SpanSeq, Position) ],
                      spans : SpanSeq,
                      position : Position )
                    ( elementFactory : ElementFactory, discounter : Discounter )
  : Unit = {
    val li = elementFactory.uli( elementFactory.para(spans, position),
                                 position )
    if ( list.isEmpty ) {
      list += elementFactory.ulist( li )
    } else {
      list.last match {
        case ul : UnorderedList => val appended = ul + li
                                   list.update( list.length - 1, appended )
        case _ => list += elementFactory.ulist( li )
      }
    }
  }
}
