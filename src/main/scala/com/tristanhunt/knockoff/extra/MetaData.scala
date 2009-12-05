package com.tristanhunt.knockoff.extra

import scala.util.parsing.input.Position
import scala.xml.{ Group, Node, NodeBuffer }

class   MetaData( val data : Map[ String, String ], val position : Position )
extends SimpleBlock {
  
  val span : SpanSeq = new Text( markdown )
  
  /** This should be an empty element. */
  val xml : Node = new Group( new NodeBuffer )
  
  val markdown : String =
    data.map{ case ((k,v)) => k + ": " + v }.mkString("\n")
}
