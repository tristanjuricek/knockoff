package knockoff2

trait SimpleBlock extends Block {
    override def theSeq : Seq[ Block ] = List( this )
}
