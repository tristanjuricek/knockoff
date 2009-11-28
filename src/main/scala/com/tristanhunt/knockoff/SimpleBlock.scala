package com.tristanhunt.knockoff

trait SimpleBlock extends Block {
    override def theSeq : Seq[ Block ] = List( this )
}
