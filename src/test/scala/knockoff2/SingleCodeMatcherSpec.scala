package knockoff2

import org.scalatest._

class SingleCodeMatcherSpec extends Spec with SpanConverterFactory {
    describe( "SingleCodeMatcher" ) {
        it( "should parse a couple of single code blocks in text" ) {
            val spans = spanConverter( Nil )(
                TextChunk("a `code1` and a `code 2`")
            )
            val expected = List(
                t("a "), codeSpan("code1"), t(" and a "), codeSpan("code 2")
            )
            assert( spans sameElements expected )
        }
    }
}
