package com.tristanhunt.knockoff

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.util.parsing.input.NoPosition

@RunWith(classOf[JUnitRunner])
class SpanConverterSpec extends FunSpecLike with Matchers {

  def convert( txt : String ) : List[Span] = convert( txt, Nil )

  def convert( txt : String, defs : Seq[LinkDefinitionChunk] ) : List[Span] =
    new SpanConverter( defs )( TextChunk(txt) ).toList

  describe("SpanConverter") {
    it( "should parse a couple of single code blocks in text" ) {
      val txt = "a `code1` and a `code 2`"
      convert( txt ) should equal {
        List( Text("a "), CodeSpan("code1"), Text(" and a "), CodeSpan("code 2") ) }
    }

    it("should not care about other elements in the code") {
      val txt = "This `code block *with em*` is OK"
      convert( txt ) should equal {
        List( Text("This "), CodeSpan( "code block *with em*" ), Text(" is OK") ) }
    }

    it("double-tick code markers should preserve whitespace") {
      val txt = "AA `` ` `` BB"
      convert( txt ) should equal {
        List( Text("AA "), CodeSpan(" ` "), Text(" BB") ) }
    }

    it("should match emphasis underscores containing asterix emphases") {
      val txt = "an _underscore *and block* em_"
      convert( txt ) should equal {
        List( Text("an "),
              Emphasis(
                List( Text("underscore "),
                      Emphasis( Text("and block") :: Nil ),
                      Text(" em") ) ) ) }
    }

    it("should match strong underscores containing asterix emphases") {
      val txt = "an __underscore **and asterix** strong__"
      convert( txt ) should equal {
        List( Text("an "),
              Strong( List( Text("underscore "),
                            Strong( List(Text("and asterix")) ),
                            Text(" strong") ) ) )
      }
    }

    it("should find an <a> and an <img>") {
      val txt = """with <a href="http://example.com">a link</a> and an <img src="foo.img"/> ha!"""
      convert( txt ) should equal {
        List( Text("with "),
              HTMLSpan("""<a href="http://example.com">a link</a>"""),
              Text(" and an "), HTMLSpan("""<img src="foo.img"/>"""),
              Text(" ha!") ) }
    }

    it("should wrap a <span> that contains another <span>") {
      val txt = """a <span class="container">contains <span>something</span>
                  | else</span> without a problem <br /> !""".stripMargin
      convert( txt ) should equal {
        List( Text("a "),
              HTMLSpan( """<span class="container">contains """ +
                        "<span>something</span>\n else</span>" ),
              Text(" without a problem "), HTMLSpan("<br />"), Text(" !") ) }
    }

    it("should find a couple of entities and pass them through") {
      val txt = "an &amp; and an &em; are in here"
      convert( txt ) should equal {
        List( Text("an "), HTMLSpan("&amp;"), Text(" and an "), HTMLSpan("&em;"),
              Text(" are in here") ) }
    }

    it("should handle HTML headers defined in text") {
      val txt = "<h2 id=\"overview\">Overview</h2>"
      convert( txt ) should equal {
        List( HTMLSpan("<h2 id=\"overview\">Overview</h2>") ) }
    }

    it("should discover inline, image, automatic, and reference links") {
      val defs = List( new LinkDefinitionChunk( "link1", "http://example.com",
                                                Some("title") ) )
      val txt = "A [link](http://example.com/link1) " +
                "An ![image link](http://example.com/image1 \"image test\") " +
                "The <http://example.com/automatic> A [reference link] [link1]"
      convert( txt, defs ) should equal {
        List( Text("A "),
              Link( List(Text("link")), "http://example.com/link1", None ),
          		Text(" An "),
              ImageLink( List(Text("image link")), "http://example.com/image1",
                         Some("image test") ),
              Text(" The "),
              Link( List(Text("http://example.com/automatic")),
                    "http://example.com/automatic", None ),
              Text(" A "),
              Link( List(Text("reference link")), "http://example.com",
                    Some("title") ) ) }
    }

    it("should hande link references in different case") {
      val defs = List( new LinkDefinitionChunk( "link 1", "http://example.com/1",
                                                None ),
                       new LinkDefinitionChunk( "link 2", "http://example.com/2",
                                                None ) )
      val txt = "[Link 1][] and [link 2][]"
      convert( txt, defs ) should equal {
        List( Link( List(Text("Link 1")), "http://example.com/1", None ),
              Text(" and "),
              Link( List(Text("link 2")), "http://example.com/2", None ) ) }
    }

    it("should escape asterixes in content") {
      val txt = """an \*escaped\* emphasis"""
      convert( txt ) should equal( List( Text("""an \*escaped\* emphasis""") ) )
    }

    it("should escape backticks in content") {
      val txt = """an escaped \' backtick"""
      convert( txt ) should equal( List( Text("""an escaped \' backtick""") ) )
    }

    it("should ignore backslashes in code") {
      val txt = """a backslash `\` in code"""
      convert( txt ) should equal(
        List( Text("""a backslash """), CodeSpan("\\"), Text(""" in code""") ) )
    }

    it("should parse links ending in brackets") {
      var text = """a [link](http://example.com/path_(foo))"""
      var parsed = List( Text("a "),
                         Link( List(Text("link")),
                               "http://example.com/path_(foo)",
                               None))
      convert(text) should equal (parsed)
    }
  }
}
