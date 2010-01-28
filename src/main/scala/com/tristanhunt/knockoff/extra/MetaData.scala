package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ SpanSeq, SimpleBlock, Text }
import scala.util.parsing.input.Position
import scala.xml.{ Group, Node, NodeBuffer }


class   MetaData( val data : Map[ String, String ], val position : Position )
extends SimpleBlock {
  
  val span : SpanSeq = new Text( markdown )
  
  /** This should be an empty element. */
  val xml : Node = new Group( new NodeBuffer )
  
  val markdown : String =
    data.map{ case ((k,v)) => k + ": " + v }.mkString("\n")
  
  override def toString : String = "MetaData(" +
    Seq( data, position ).mkString("/") +
  ")"
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case oth : MetaData =>
      ( oth.canEqual( this ) ) &&
      ( data sameElements oth.data ) &&
      ( position == oth.position )
    case _ => false
  }
  
  def canEqual( rhs : MetaData ) : Boolean =
    rhs.isInstanceOf[ MetaData ]
  
  override def hashCode : Int = 41 * ( data.hashCode + 41 * position.hashCode )
}
