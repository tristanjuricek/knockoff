package com.tristanhunt.knockoff.extra

trait Wholesaler extends Discounter with MetaDataConverter {
  
  override def knockoff( source : java.lang.CharSequence ) : BlockSeq = {
    var blocks = super.knockoff( source )
    
    if ( ! blocks.isEmpty ) {
      blocks.first match {
        case p : Paragraph =>
          toMetaData( p ).foreach { metaData =>
            blocks = new GroupBlock( metaData :: blocks.toList.tail ) }
        case _ => {}
      }
    }
    
    return blocks
  }
}
