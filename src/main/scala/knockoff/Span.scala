package knockoff

import scala.xml.Node

trait Span extends SpanSeq {
  def markdown : String
  def xml : Node
}

object Span {
  val empty : Span = new Text("")
}
