package knockoff2

import scala.io.Source
import scala.util.parsing.input.{ NoPosition, Position }

/**
 * @param ordered Alters the output, mostly.
 */
class MarkdownList(
    val ordered  : Boolean,
    val children : BlockSeq
) extends ComplexBlock {
    
    val position = children.firstOption match {
        case None => NoPosition
        case Some( child ) => child.position
    }
    
    def xml = ordered match {
        case true  => <ol>{ childrenXML }</ol>
        case false => <ul>{ childrenXML }</ul>
    }
    
    def markdown = childrenMarkdown
    
    override def toString = "MarkdownList(" + markdown + ")"
    
    override def hashCode : Int = position.hashCode + 47
    
    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : MarkdownList => t.canEqual( this ) && ( t sameElements this )
        case _ => false
    }
    
    def sameElements( ml : MarkdownList ) : Boolean = {
        ( ordered == ml.ordered ) &&
        ( children sameElements ml.children )
    }
    
    def canEqual( t : MarkdownList ) : Boolean = t.getClass == getClass
}
