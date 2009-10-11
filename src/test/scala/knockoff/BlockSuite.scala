package knockoff

import scala.util.parsing.input.NoPosition
import org.scalatest._
import matchers._

class BlockSuite extends Spec with ShouldMatchers with HasElementFactory {

  val factory = elementFactory
  import factory._

  describe("BlockSeq") {

    it( "should filter Paragraphs and Headers properly with ?" ) {

      val p1 = para( t("p1"), NoPosition )
      val h1 = head( 1, t("h1"), NoPosition )

      val blocks = BlockSeq.fromSeq( List( p1, h1 ) )
      
      ( blocks ? Paragraphs ) should have length (1)
      assert( ( blocks ? Paragraphs ) contains p1 )
      
      ( blocks ? Headers ) should have length (1)
      assert( ( blocks ? Headers ) contains h1 )
    }
  }
  
  describe("MarkdownList") {
    
    it("should implement simple lists") {
        
    }
    
    it("should implement complex lists") {
        
    }
  }
}
