package knockoff2

trait SpanMatcher {

    def recursive = true
    
    def find(
            str     : String,
            convert : String => Span
        ) ( implicit
            factory : ElementFactory
        ) : Option[ SpanMatch ]
}
