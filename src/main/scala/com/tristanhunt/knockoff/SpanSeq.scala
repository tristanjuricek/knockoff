package com.tristanhunt.knockoff

import scala.xml.Group

trait SpanSeq extends Seq[ Span ] {
  def theSeq : Seq[ Span ]
  override def length : Int = theSeq.length
  override def elements = theSeq.elements
  override def apply( ii : Int ) = theSeq(ii)
  
  def toXML = Group( theSeq.flatMap( _.xml ) )
  
  def toMarkdown = theSeq.map( _.markdown ).mkString("")
}
