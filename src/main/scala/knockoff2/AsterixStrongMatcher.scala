package knockoff2

object  AsterixStrongMatcher
extends EqualDelimiterMatcher(
    "**",
    (i,b,c,a,f) => SpanMatch( i, b, f.strong(c), a )
)
