package knockoff2

import scala.util.parsing.input.Position

class OrderedItem( children : BlockSeq, position : Position )
extends ListItem( children, position ) {

  def this( block : Block, position : Position ) =
    this( new BlockSeq{ val theSeq = Seq( block ) }, position )
  
  def itemPrefix = "1. "
  
  def + ( b : Block ) : OrderedItem =
    new OrderedItem( children, children.first.position )        
}
