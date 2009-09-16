package knockoff2

object InlineHTMLSplitter extends SpanMatcher with StringExtras {
    
    val startElement = """<[ ]*([a-zA-Z0-9_:]+)[ ]*[\w="'&&[^>]]*[ ]*(/?)>""".r
    
    def find(
            str     : String,
            convert : String => Span
        ) ( implicit
            factory : ElementFactory
        ) : Option[ SpanMatch ] = {
            
        import factory.{ htmlSpan, text }
            
        startElement.findFirstMatchIn( str ) match {

            case None => None
         
            case Some( open ) => {
                val hasEnd = open.group(2) == "/"
                if ( hasEnd ) {
                    Some( SpanMatch(
                        open.start,
                        if ( open.before.length > 0 ) None else Some( text( open.before.toString ) ),
                        htmlSpan( open.matched ),
                        if ( open.after.length > 0 ) None else Some( open.after.toString )
                    ) )
                } else {
                    val closer = ("(?i)</[ ]*" + open.group(1) + "[ ]*>").r
                    closer.findFirstMatchIn( open.after ) match {
                        
                        case None => None
                        
                        case Some( close ) => Some( SpanMatch(
                            open.start,
                            if ( open.before.length > 0 ) None else Some( text( open.before.toString ) ),
                            htmlSpan( str.substring( open.start, close.end ) ),
                            if ( close.after.length > 0 ) None else Some( close.after.toString )
                        ) )
                    }
                }
            }
        }
    }
}
