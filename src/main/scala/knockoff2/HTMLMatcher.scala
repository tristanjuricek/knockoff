package knockoff2

object InlineHTMLSplitter extends SpanMatcher with StringExtras {
    
    val startElement = """<[ ]*([a-zA-Z:_]+)[ \t]*[^>]*?(/?+)>""".r
    
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
                    val leading = {
                        if ( open.before.length > 0 )
                            Some( text( open.before.toString ) )
                        else
                            None
                    }
                    val trailing = {
                        if ( open.after.length > 0 ) Some( open.after.toString )
                        else None
                    }
                    Some( SpanMatch(
                        open.start,
                        leading,
                        htmlSpan( open.matched ),
                        trailing
                    ) )
                } else {
                    val closer = ("(?i)</[ ]*" + open.group(1) + "[ ]*>").r
                    closer.findFirstMatchIn( open.after ) match {
                        
                        case None => None
                        
                        case Some( close ) => {
                            val leading = {
                                if ( open.before.length > 0 )
                                    Some( text( open.before.toString ) )
                                else
                                    None
                            }
                            val trailing = {
                                if ( close.after.length > 0 )
                                    Some( close.after.toString )
                                else
                                    None
                            }
                            Some( SpanMatch(
                                open.start,
                                leading,
                                htmlSpan( str.substring( open.start, open.end + close.end ) ),
                                trailing
                            ) )
                        }
                    }
                }
            }
        }
    }
}
