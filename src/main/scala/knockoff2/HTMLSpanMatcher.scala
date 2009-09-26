package knockoff2

trait HTMLMatchers { self : SpanConverter =>
  
  private val startElement = """<[ ]*([a-zA-Z:_]+)[ \t]*[^>]*?(/?+)>""".r
  
  def matchHTMLSpan( source : String ) : Option[ SpanMatch ] = {
    startElement.findFirstMatchIn( source ).map { open =>
      val hasEnd = open.group(2) == "/"
      if ( hasEnd ) {
        SpanMatch(
          open.start,
          open.before.toOption.map( elementFactory.text(_) ),
          elementFactory.htmlSpan( open.matched ),
          open.after.toOption
        )
      } else {
        hasMatchedClose( source, open.group(1), open.end, 1 ).map {
          closeAndAfter => SpanMatch(
            open.start,
            open.before.toOption.map( elementFactory.text(_) ),
            elementFactory.htmlSpan(
              source.substring( open.start, closeAndAfter._1 )
            ),
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
    
    val nextOpen  = opener.findFirstMatchIn( source.substring(from) )
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
  
  private val matchEntityRE = """&\w+;""".r

  def matchEntity( source : String ) : Option[ SpanMatch ] = {
    matchEntityRE.findFirstMatchIn( source ).map{ entityMatch =>
      SpanMatch(
        entityMatch.start,
        entityMatch.before.toOption.map( elementFactory.text(_) ),
        elementFactory.htmlSpan( entityMatch.matched ),
        entityMatch.after.toOption
      )
    }
  }
}
