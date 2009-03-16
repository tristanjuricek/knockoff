package com.tristanhunt.knockoff

protected object InlineHTMLSplitter {
 
    import util.matching._
 
    val starter = new Regex("<[ ]*([a-zA-Z]+)[ ]*>")
    
    def split(str:String):List[Nad] = {
        
        starter.findFirstMatchIn(str) match {
            case Some(matchStart) => {
                val ender = new Regex("(?i)</[ ]*" + matchStart.group(1) + "[ ]*>")
                ender.findFirstMatchIn(matchStart.after) match  {
                    case Some(matchEnd) => {
                        List(Text(matchStart.before.toString),
                            HTML(str.substring(matchStart.start, matchStart.end(1) + matchEnd.end + 1).toString)) :::
                            split(matchEnd.after.toString)
                    }
                    
                    case None => List(Text(str))
                }
            }
            case None => List(Text(str))
        }
    }
}
