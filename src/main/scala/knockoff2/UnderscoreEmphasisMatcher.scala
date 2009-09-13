package knockoff2

object  UnderscoreEmphasisMatcher
extends EqualDelimiterMatcher(
    "_",
    (i,b,c,a,f) => SpanMatch( i, b, f.em(c), a )
)
