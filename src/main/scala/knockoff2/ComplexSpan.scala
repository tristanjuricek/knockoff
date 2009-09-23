package knockoff2

trait ComplexSpan extends Span {
  val children : SpanSeq
  def theSeq = children
  def childrenMarkdown = children.map( _.markdown ).mkString("")
  def childrenXML = children.map( _.xml )
}
