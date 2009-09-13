package knockoff2

trait ComplexBlock extends Block {
    val children : Seq[ Block ]
    val span = new GroupSpan( children.map( _.span ) )
    def theSeq = children
    def childrenMarkdown = children.map( _.markdown ).mkString("\n")
    def childrenXML = children.map( _.xml )
}
