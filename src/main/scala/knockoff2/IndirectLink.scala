package knockoff2

class IndirectLink(
  children        : SpanSeq,
  val definition  : LinkDefinition
)
extends Link( children, definition.url, definition.title )
with    ComplexSpan {
    
  override def markdown = "[" + childrenMarkdown + "][" + definition.id + "]"
    
  override def toString = "IndirectLink(" + markdown + ")"
  
  override def hashCode : Int =
    41 + ( (7 /: children)( (sum, child) => 41 + sum + 7 * child.hashCode ) )
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : IndirectLink => ( t.canEqual( this ) ) && ( this sameElements t )
    case _ => false
  }
  
  def canEqual( s : IndirectLink ) : Boolean = s.getClass == getClass
}
