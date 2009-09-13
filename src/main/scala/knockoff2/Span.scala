package knockoff2

import scala.xml.Node

trait Span extends SpanSeq {
    def markdown : String
    def xml : Node
}

object Span extends ElementFactory {
    val empty : Span = t("")
}
