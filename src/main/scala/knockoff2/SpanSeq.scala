package knockoff2

trait SpanSeq extends Seq[ Span ] {
  def theSeq : Seq[ Span ]
  override def length : Int = theSeq.length
  override def elements = theSeq.elements
  override def apply( ii : Int ) = theSeq(ii)
}
