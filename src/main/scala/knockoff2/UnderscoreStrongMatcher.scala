package knockoff2

object  UnderscoreStrongMatcher
extends EqualDelimiterMatcher(
    "__",
    (i,b,c,a,f) => SpanMatch( i, b, f.strong(c), a )
)
