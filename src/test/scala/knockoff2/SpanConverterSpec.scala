package knockoff2

import org.scalatest._
import org.scalatest.matchers._

class   SpanConverterSpec
extends Spec
with    ShouldMatchers
with    SpanConverterFactory
with    ColoredLogger {
  
  val factory = elementFactory
  import factory._

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
  }
}
