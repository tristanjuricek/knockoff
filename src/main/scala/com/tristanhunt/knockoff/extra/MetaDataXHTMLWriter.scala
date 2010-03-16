package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff._
import scala.xml.{ Group, Node }

trait MetaDataXHTMLWriter extends XHTMLWriter {
 
  override def blockToXHTML : Block => Node = block => block match {
    case MetaData( data, _ ) => Group( Nil )
    case _ => super.blockToXHTML( block )
  }
}
