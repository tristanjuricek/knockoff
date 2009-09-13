package knockoff2

import scala.xml.Elem
import scala.util.parsing.input.Position

class Paragraph(
    val span        : Span,
    val position    : Position
)
extends SimpleBlock {

    def markdown = span.markdown

    def xml = <p>{ span.xml }</p>
    
    override def toString = "Paragraph(" + markdown + ")"
    
    override def equals( rhs : Any ):Boolean = rhs match {
        case oth : Paragraph => ( oth canEqual this ) && ( this sameElements oth )
        case _ => false
    }
    
    def canEqual( p : Paragraph ) : Boolean = ( getClass == p.getClass )
    
    def sameElements( p : Paragraph ) : Boolean = {
        ( span == p.span ) &&
        ( position == p.position )
    }
    
    override def hashCode : Int = span.hashCode + position.hashCode
}
