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
  
  describe("StringExtras.countLeading") {

    it("should be ok with nothing to match") {
      "no leading".countLeading('#') should equal (0)
      "".countLeading('#') should equal (0)
    }
    
    it("should be fine with only these characters") {
      "###".countLeading('#') should equal (3)
    }
    
    it("should handle only the characters up front") {
      "## unbalanced #".countLeading('#') should equal (2)
    }
  }
  
  describe("StringExtras.trim(ch)") {
   
    it("should remove likely headers with the match char inside") {
      "## Who does #2 work for? #".trim('#').trim should equal (
        "Who does #2 work for?"
      )
    }
  }
}
