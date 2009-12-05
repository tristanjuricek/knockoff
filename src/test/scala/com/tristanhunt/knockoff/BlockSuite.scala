package com.tristanhunt.knockoff

import scala.util.parsing.input.NoPosition
import org.scalatest._
import matchers._

class BlockSuite extends Spec with ShouldMatchers with HasElementFactory {

  val factory = elementFactory
  import factory._

  describe("BlockSeq") {

    it( "should filter Paragraphs and Headers properly with filterType" ) {

      val p1 = para( t("p1"), NoPosition )
      val h1 = head( 1, t("h1"), NoPosition )

      val blocks = BlockSeq.fromSeq( List( p1, h1 ) )
      
      ( blocks filterType Paragraphs ) should have length (1)
      assert( ( blocks filterType Paragraphs ) contains p1 )
      
      ( blocks filterType Headers ) should have length (1)
      assert( ( blocks filterType Headers ) contains h1 )
    }
  }
}
