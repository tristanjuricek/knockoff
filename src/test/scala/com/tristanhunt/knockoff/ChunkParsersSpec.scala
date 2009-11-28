package com.tristanhunt.knockoff

import org.scalatest._
import org.scalatest.matchers._

class ChunkParsersSpec extends ChunkParser with Spec with ShouldMatchers {

  describe("ChunkParser") {
    it("should handle simple bullet items") {
      val src = "* item 1\n* item 2\n"
      parse( chunk, src ).get should equal ( BulletLineChunk("item 1\n") )
    }
    
    it("should group a second line that's not a bullet") {
      val src = "*   item 1\n    more\n"
      parse( chunk, src ).get should equal (
        BulletLineChunk("item 1\nmore\n")
      )
    }
    
    it("should ignore whitespace around headers") {
      val src = "# Header 1 #"
      parse( chunk, src ).get should equal { HeaderChunk(1, "Header 1") }
    }
    
    it("should be ok with empty code blocks") {
      val src = "    "
      parse( chunk, src ).get should equal { IndentedChunk("") }
    }
    
    it("should not explode on a code block with a trailing line") {
      val line = "    line\n    "
      parse( chunk, line ).get should equal { IndentedChunk("line\n") }
    }
    
    it("should handle nothin' but code") {
      val src = "    This is just a code block.\n" +
                "    \n" +
                "    And it has a trailing whitespace line... that's also indented.\n" +
                "    "
      parse( chunk, src ).get should equal { IndentedChunk(
        "This is just a code block.\n" +
        "\n" +
        "And it has a trailing whitespace line... that's also indented.\n"
      ) }
    }
  }
}
