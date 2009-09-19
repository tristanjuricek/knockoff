package knockoff2

object InlineHTMLMatcher extends SpanMatcher with StringExtras with ColoredLogger {
    
    val startElement = """<[ ]*([a-zA-Z:_]+)[ \t]*[^>]*?(/?+)>""".r
    
    def find(
            str     : String,
            convert : String => Span
        ) ( implicit
            factory : ElementFactory
        ) : Option[ SpanMatch ] = {
            
        import factory.{ htmlSpan, text }
        
        startElement.findFirstMatchIn( str ).map { open =>
            
            val hasEnd = open.group(2) == "/"

            if ( hasEnd ) {
                SpanMatch(
                    open.start,
                    open.before.toOption.map( text(_) ),
                    htmlSpan( open.matched ),
                    open.after.toOption
                )
            } else {
                hasMatchedClose( str, open.group(1), open.end, 1 ).map {
                    closeAndAfter => SpanMatch(
                        open.start,
                        open.before.toOption.map( text(_) ),
                        htmlSpan( str.substring( open.start, closeAndAfter._1 ) ),
                        closeAndAfter._2.toOption
                    )
                }.getOrElse( return None )
            }
        }
    }
    
    private def hasMatchedClose(
            source : String,
            tag    : String,
            from   : Int,
            opens  : Int
        ) : Option[ ( Int, CharSequence ) ] = {
        
        val opener = ("(?i)<[ ]*" + tag + "[ \t]*[^>]*?(/?+)*>").r
        val closer = ("(?i)</[ ]*" + tag + "[ ]*>").r
        
        val nextOpen = opener.findFirstMatchIn( source.substring(from) )
        val nextClose = closer.findFirstMatchIn( source.substring(from) )

        if ( ! nextClose.isDefined ) return None
        
        if ( nextOpen.isDefined && ( nextOpen.get.start < nextClose.get.start ) ) {
            hasMatchedClose( source, tag, from + nextOpen.get.end, opens + 1 )
        } else if ( opens > 1 ) {
            hasMatchedClose( source, tag, from + nextClose.get.end, opens - 1 )
        } else {
            Some( ( from + nextClose.get.end, nextClose.get.after ) )
        }
    }
}
