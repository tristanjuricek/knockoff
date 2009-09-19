package knockoff2

import org.scalatest._
import org.scalatest.matchers._

class InlineHTMLMatcherSpec extends Spec with ShouldMatchers with SpanConverterFactory {

    describe("InlineHTMLMatcher") {
     
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
    }
}
