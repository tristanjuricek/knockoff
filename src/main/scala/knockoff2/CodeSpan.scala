package knockoff2

import scala.xml.Node

class CodeSpan( val content : String ) extends SimpleSpan {

    def markdown = content

    def xml : Node = <code>{ content }</code>

    override def toString = "CodeSpan(" + content + ")"
    
    override def hashCode : Int = content.hashCode
        
    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : CodeSpan => t.canEqual( this ) && ( t.content == content )
        case _ => false
    }
        
    def canEqual( t : CodeSpan ) : Boolean = t.getClass == getClass
}
