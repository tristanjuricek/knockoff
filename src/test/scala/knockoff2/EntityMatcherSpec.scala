package knockoff2

import org.scalatest._
import org.scalatest.matchers._

class EntityMatcherSpec extends Spec with ShouldMatchers with SpanConverterFactory {
    describe("EntityMatcher") {
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
