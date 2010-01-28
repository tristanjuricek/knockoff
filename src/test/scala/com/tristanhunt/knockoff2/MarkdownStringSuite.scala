package com.tristanhunt.knockoff2

import org.scalatest._
import org.scalatest.matchers._    
import MarkdownString._

class MarkdownStringSuite extends Spec with ShouldMatchers {
  
  describe("MarkdownString") {        
    import MarkdownString._
    
    it("should safely enable string.trimEnds") {
      "%foo%".trimEnds should equal ("foo")
      "1".trimEnds should equal ("1")
      "11".trimEnds should equal ("")
    }
    
    it("should replace all backslashes in escapeable content") {
      """Plus \+""".unescape should equal("Plus +")
      """Backslash \\""".unescape should equal("Backslash \\")
    }
  }
}

