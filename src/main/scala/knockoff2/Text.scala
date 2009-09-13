package knockoff2

import scala.xml.{ Node, Text => XMLText }

class Text( val content : String ) extends SimpleSpan {

    def markdown = content

    def xml : Node = XMLText( content )

    override def toString = "Text(" + content + ")"
    
    override def hashCode : Int = content.hashCode
        
    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : Text => t.canEqual( this ) && ( t.content == content )
        case _ => false
    }
        
    def canEqual( t : Text ) : Boolean = t.getClass == getClass
}
