package knockoff2

object SingleCodeMatcher
extends CodeDelimiterMatcher(
    "`",
    (i,b,c,a,f) =>
        SpanMatch( i, b, f.codeSpan( c.asInstanceOf[ Text ].content ), a )
)
