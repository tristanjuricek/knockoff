package com.tristanhunt.knockoff

trait ComplexSpan extends Span {
  val children : Seq[ Span ]
  def theSeq = children
  def childrenMarkdown = children.map( _.markdown ).mkString("")
  def childrenXML = toXML
}
