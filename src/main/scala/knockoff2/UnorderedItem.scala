package knockoff2

import scala.util.parsing.input.Position
    
class UnorderedItem( children : BlockSeq, position : Position )
extends ListItem( children, position ) {
  
  def this( block : Block, position : Position ) =
    this( new BlockSeq{ val theSeq = Seq( block ) }, position )
  
  def itemPrefix = "* "
  
  def + ( b : Block ) : ListItem =
    new UnorderedItem( new GroupBlock( children ++ Seq(b) ), children.first.position )
}
