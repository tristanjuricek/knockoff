package knockoff

trait ComplexBlock extends Block {
    val children : Seq[ Block ]
    val span = new SpanSeq{ def theSeq = children.flatMap( _.span ) }
    def theSeq = children
    def childrenMarkdown = children.map( _.markdown ).mkString("\n") + "\n"
    def childrenXML = children.map( _.xml )
}
