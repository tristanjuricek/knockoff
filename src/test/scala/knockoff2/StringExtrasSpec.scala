package knockoff2

import org.scalatest._
import org.scalatest.matchers._

class StringExtrasSpec extends Spec with ShouldMatchers with ColoredLogger
  with StringExtras {
    
  describe("StringExtras.nextNIndices") {

    it( "should find two different groups of the same time" ) {
      "a `foo` b `bar`".nextNIndicesOf(2,"`") should equal ( List( 2, 6 ) )
    }

    it( "should deal with only one index" ) {
      "a `foo with nothin'".nextNIndicesOf(2, "`") should equal (Nil)
    }
  }
}
