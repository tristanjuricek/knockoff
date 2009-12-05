package com.tristanhunt.knockoff.extra

trait Wholesaler extends Discounter with MetaDataConverter {
  
  override def knockoff( source : java.lang.CharSequence ) : Seq[ Block ] = {
    var blocks = super.knockoff( source )
    
    if ( ! blocks.isEmpty ) {
      blocks.first match {
        case p : Paragraph =>
          toMetaData( p ).foreach { metaData =>
            blocks = Seq( metaData ) ++ blocks.drop(1) }
        case _ => {}
      }
    }
    
    return blocks
  }
}
