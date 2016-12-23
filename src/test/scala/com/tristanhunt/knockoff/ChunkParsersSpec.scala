package com.tristanhunt.knockoff

//import org.junit.runner.RunWith
import org.scalatest.junit._
import org.junit.runner._
import org.scalatest.matchers._
import org.scalatest._

@RunWith(classOf[JUnitRunner])
class ChunkParsersSpec extends ChunkParser with FunSpecLike with Matchers {

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
    
    it("should deal with a CRLF") {
      val src = "This is a line \r\nthat is broken in\r\n a couple places.\r\n"
      parse( chunk, src ).get should equal { TextChunk(src) }
    }
    
    it("should deal with just a CRLF") {
      val src = "\u000d\u000a"
      parse( chunk, src ).get should equal { EmptySpace(src) }
    }
  }
}
