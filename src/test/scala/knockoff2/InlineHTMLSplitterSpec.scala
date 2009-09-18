package knockoff2

import org.scalatest._
import org.scalatest.matchers._

class InlineHTMLSplitterSpec extends Spec with ShouldMatchers with SpanConverterFactory {

    describe("InlineHTMLSplitter") {
     
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
    }
}
