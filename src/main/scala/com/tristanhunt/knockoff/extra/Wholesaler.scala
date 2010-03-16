package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ Block, Paragraph, Discounter }
import com.tristanhunt.knockoff.latex.{ LatexDiscounter, LatexWriter }

trait Wholesaler extends Discounter with MetaDataConverter
with MetaDataXHTMLWriter with LatexDiscounter with LatexWriter {
  
  override def knockoff( source : java.lang.CharSequence ) : Seq[ Block ] = {
    var blocks = super.knockoff( source )
    
    if ( ! blocks.isEmpty ) {
      blocks.first match {
        case p : Paragraph =>
          toMetaData( p ).foreach { metaData =>
            blocks = List( metaData ) ++ blocks.drop(1) }
        case _ => {}
      }
    }
    
    return blocks
  }
}
