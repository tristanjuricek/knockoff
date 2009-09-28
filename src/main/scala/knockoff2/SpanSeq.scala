package knockoff2

import scala.xml.Group

trait SpanSeq extends Seq[ Span ] {
  def theSeq : Seq[ Span ]
  override def length : Int = theSeq.length
  override def elements = theSeq.elements
  override def apply( ii : Int ) = theSeq(ii)
  
  def toXML = Group( theSeq.map( _.xml ) )
  
  def toMarkdown = theSeq.map( _.markdown ).mkString("")
}
