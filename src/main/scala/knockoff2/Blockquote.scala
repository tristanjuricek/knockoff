package knockoff2

import scala.io.Source
import scala.xml.Elem
import scala.util.parsing.input.Position

class Blockquote(
    val children : BlockSeq,
    val position : Position
)
extends ComplexBlock {
            
    def markdown : String = {
        Source.fromString( childrenMarkdown ).getLines.map { line =>
            "> " + line
        }.mkString( "" )
    }
    
    def xml : Elem = <blockquote>{ childrenXML }</blockquote>
    
    override def toString = "Blockquote(" + markdown + ")"
    
    override def equals( rhs : Any ):Boolean = rhs match {
        case oth : Blockquote => ( oth canEqual this ) && ( this sameElements oth )
        case _ => false
    }
    
    def canEqual( b : Blockquote ) : Boolean = ( getClass == b.getClass )
    
    def sameElements( b : Blockquote ) = {
        ( children sameElements b.children ) &&
        ( position == b.position )
    }
    
    override def hashCode : Int = {
        43 + ( ( 3 /: children )( (sum, child) => {
            sum + 43 + 3 * child.hashCode
        } ) )
    }
}
