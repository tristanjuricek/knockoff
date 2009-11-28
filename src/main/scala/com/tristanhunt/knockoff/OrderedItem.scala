package com.tristanhunt.knockoff

import scala.util.parsing.input.Position

class OrderedItem( items : Seq[ Block ], position : Position )
extends ListItem( items, position ) {

  def this( block : Block, position : Position ) =
    this( Seq( block ), position )
  
  def itemPrefix = "1. "
  
  def + ( b : Block ) : ListItem =
    new OrderedItem( children ++ Seq(b), children.first.position )
}
