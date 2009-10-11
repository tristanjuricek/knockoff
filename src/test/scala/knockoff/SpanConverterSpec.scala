package knockoff

import org.scalatest._
import org.scalatest.matchers._
import scala.util.parsing.input.NoPosition

class   SpanConverterSpec
extends Spec
with    ShouldMatchers
with    SpanConverterFactory
with    ColoredLogger {
  
  val factory = elementFactory
  import factory._
  
  override def spanConverter( definitions : Seq[ LinkDefinitionChunk ] ) : Chunk => SpanSeq =
    new SpanConverter( definitions, elementFactory ) with ColoredLogger

  describe( "CodeMatchers" ) {
  
    it( "should parse a couple of single code blocks in text" ) {
      val spans = spanConverter( Nil )(
        TextChunk("a `code1` and a `code 2`")
      )
      val expected = List(
        t("a "), codeSpan("code1"), t(" and a "), codeSpan("code 2")
      )
      assert( spans sameElements expected )
    }
  
    it("should not care about other elements in the code") {
      val converted = spanConverter( Nil )(
        TextChunk("This `code block *with em*` is OK")
      )
      converted.toList should equal { List(
        text("This "),
        codeSpan( "code block *with em*" ),
        text(" is OK")
      ) }
    }
    
    it("double-tick code markers should preserve whitespace") {
      val converted = spanConverter(Nil)( TextChunk("AA `` ` `` BB") )
      converted.toList should equal { List(
        text("AA "),
        codeSpan(" ` "),
        text(" BB")
      ) }
    }
  }
  
  describe( "EmphasisMatchers" ) {
    it("should match underscores containing asterix emphases") {
      val converted = spanConverter( Nil )(
        TextChunk( "a _underscore *and block* em_" )
      )
      converted.toList should equal { List(
        text("a "),
        em( toSpan( List( t("underscore "), em( t("and block") ), t(" em") ) ) )
      ) }
    }
  }

  describe( "StrongMatchers" ) {
    it("should match underscores containing asterix emphases") {
      val converted = spanConverter( Nil )(
        TextChunk( "an __underscore **and asterix** strong__" )
      )
      converted.toList should equal { List(
        text("an "),
        strong(
          toSpan(
            List( t("underscore "), strong( t("and asterix") ), t(" strong") )
          )
        )
      ) }
    }
  }
  
  describe("HTMLSpanMatcher") {
    it("should find an <a> and an <img>") {
      val spans = spanConverter( Nil )( TextChunk(
        """with <a href="http://example.com">a link</a> and an """ +
        """<img src="foo.img"/> ha!"""
      ) )
      spans.toList should equal ( List(
        t("with "),
        htmlSpan("""<a href="http://example.com">a link</a>"""),
        t(" and an "),
        htmlSpan("""<img src="foo.img"/>"""),
        t(" ha!")
      ) )
    }
    
    it("should wrap a <span> that contains another <span>") {
      val convertedSpans = spanConverter( Nil ){ TextChunk(
        """a <span class="container">contains <span>something</span>""" +
        """ else</span> without a problem <br /> !"""
      ) }
      convertedSpans.toList should equal { List(
        t("a "),
        htmlSpan(
          """<span class="container">contains """ +
          """<span>something</span> else</span>"""
        ),
        t(" without a problem "),
        htmlSpan("<br />"),
        t(" !")
      ) }
    }
    
    it("should find a couple of entities and pass them through") {
      val converted = spanConverter( Nil )(
          TextChunk( "an &amp; and an &em; are in here" )
      )
      converted.toList should equal( List(
        t("an "),
        htmlSpan("&amp;"),
        t(" and an "),
        htmlSpan("&em;"),
        t(" are in here")
      ) )
    }
    
    it("should handle HTML headers defined in text") {
      val converted = spanConverter(Nil)(
          TextChunk("<h2 id=\"overview\">Overview</h2>")
      )
      converted.toList should equal( List(
        htmlSpan("<h2 id=\"overview\">Overview</h2>")
      ) )
    }
  }
  
  describe("LinkMatcher") {
    it("should discover inline, image, automatic, and reference links") {
      val convert = spanConverter(
        Seq( new LinkDefinitionChunk("link1", "http://example.com", Some("title") ) )
      )
      val converted = convert(
        TextChunk(
          "A [link](http://example.com/link1) " +
          "An ![image link](http://example.com/image1 \"image test\") " +
          "The <http://example.com/automatic> " +
          "A [reference link] [link1]"
        )
      )
      converted.toList should equal { List(
        text("A "),
        link( t("link"), "http://example.com/link1" ),
        text(" An "),
        ilink( t("image link"), "http://example.com/image1", Some("image test") ),
        text(" The "),
        link( t("http://example.com/automatic"), "http://example.com/automatic" ),
        text(" A "),
        link( t("reference link"), "http://example.com", Some("title") )
      ) }
    }
  }
  
  describe("Escaping system") {
  
    it("should escape asterixes in content") {
      val converted = spanConverter(Nil)(
        TextChunk("""an \*escaped\* emphasis""")
      )
      converted.toList should equal( List(
        text("""an \*escaped\* emphasis""")
      ) )
    }
    
    it("should escape backticks in content") {
      val converted = spanConverter(Nil)(
        TextChunk("""an escaped \' backtick""")
      )
      converted.toList should equal( List(
        text("""an escaped \' backtick""")
      ) )
    }
    
    it("should ignore backslashes in code") {
      val converted = spanConverter(Nil)(
        TextChunk("""a backslash `\` in code""")
      )
      converted.toList should equal( List(
        text("""a backslash """),
        codeSpan("\\"),
        text(""" in code""")
      ) )
    }
  }
}
