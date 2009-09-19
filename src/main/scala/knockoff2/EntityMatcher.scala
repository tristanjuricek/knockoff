package knockoff2

object EntityMatcher extends SpanMatcher with StringExtras {
    override def recursive = false

    val matchEntity = """&\w+;""".r

    def find(
            str     : String,
            convert : String => Span
        ) ( implicit
            factory : ElementFactory
        ) : Option[ SpanMatch ] = {
            
        matchEntity.findFirstMatchIn( str ) match {
            
            case None => None
            
            case Some( entityMatch ) => Some(
                SpanMatch(
                    entityMatch.start,
                    entityMatch.before.toOption.map( factory.text(_) ),
                    factory.htmlSpan( entityMatch.matched ),
                    entityMatch.after.toOption
                )
            )
        }
    }
    

}
