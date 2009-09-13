package knockoff2

object   AsterixEmphasisMatcher
extends EqualDelimiterMatcher(
    "*",
    (i,b,c,a,f) => SpanMatch( i, b, f.em(c), a )
)
