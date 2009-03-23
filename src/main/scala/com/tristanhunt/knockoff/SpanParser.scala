package com.tristanhunt.knockoff

/**
 * Parses Markdown span elements.
 *
 * @author Tristan Juricek <mr.tristan@gmail.com>
 */
class SpanParser {

    /**
     * How we order the best split done by the parse function. It's fun! (erh...)
     *
     * 1. Prefer the lower index of the first non-Text Nad.
     * 2. If the indexes are the same, then prefer the smaller size of the first element, because
     *    that first element is going to be a Text Nad, and that will be the more logical split.
     */
    private implicit def _createOrderedForSplitters(list:List[Nad]) = new Ordered[List[Nad]] {

        def nonTextIndexOf(nads:List[Nad]):Int = {
            val idx = nads.findIndexOf(n => n.isInstanceOf[Text] == false)
            return if (idx == -1) 4 else idx
        }
        
        val nonTextIndex = new runtime.RichInt(nonTextIndexOf(list))
        
        val valueLen = new runtime.RichInt(list.head.value.length)
        
        def compare(other:List[Nad]):Int = {
            
            val otherNonTextIndex = nonTextIndexOf(other)

            var cmp = 0

            if (nonTextIndex.compare(otherNonTextIndex) == 0)
                cmp = -valueLen.compare(other.head.value.length)
            else
                cmp = -nonTextIndex.compare(otherNonTextIndex)
                
            cmp
        }
    }

    /**
     * Splitters break text into: lead text, non-text-thingy, trailing-text. At each step of the way
     * we split the source in all ways possible, then take the one with the smallest leading text.
     */
    def parse(source:String):List[Nad] = {
        
        val pq = new collection.mutable.PriorityQueue[List[Nad]]
        
        pq += InlineHTMLSplitter.split(source)
        
        pq += StrongUnderscoreSplitter(this).split(source)

        pq += StrongAsterixSplitter(this).split(source)

        pq += EmphasisUnderscoreSplitter(this).split(source)

        pq += EmphasisAsterixSplitter(this).split(source)
        
        pq += InlineLinkSplitter(this).split(source)
        
        pq += InlineImageLinkSplitter(this).split(source)

        val bestSplit = pq.dequeue
        
        if (bestSplit.length == 1 || bestSplit.last.isInstanceOf[Text] == false)
            return bestSplit
        
        return bestSplit.take(bestSplit.length - 1) ::: parse(bestSplit.last.value)
    }
}

object SpanParser {
 
    def apply:SpanParser = new SpanParser
}

import util.matching._

protected trait MatchedSplitter {
    def start:Regex
    def end:Regex

    type Thing <: Nad
    
    def spanParser:SpanParser
    
    def construct(seq:Seq[Nad]):Thing

    def split(str:String):List[Nad] = {
        
        start.findFirstMatchIn(str) match {
            
            case Some(matchStart) => {
                end.findFirstMatchIn(matchStart.after(1)) match  {
                    case Some(matchEnd) => {
                        val nads = new collection.mutable.Queue[Nad]
                        if (matchStart.before(1).length > 0)
                            nads += Text(matchStart.before.toString)
                        nads += construct(
                            spanParser.parse(
                                str.substring(matchStart.end(1),
                                    matchStart.end(1) + matchEnd.start(1)).toString))
                        if (matchEnd.after(1).length > 0)
                            nads += Text(matchEnd.after(1).toString)
                        nads.toList
                    }
                    case None => List(Text(str))
                }
            }
            
            case None => List(Text(str))
        }
    }
    
}

protected trait EmphasisSplitter
extends MatchedSplitter {

    type Thing = Emphasis
    
    def construct(seq:Seq[Nad]) = Emphasis(seq)
}

protected case class EmphasisUnderscoreSplitter(val spanParser:SpanParser)
extends EmphasisSplitter {
        
    val start = new Regex("""(_)[\S&&[^_]]+""")
    
    val end = new Regex("""[\S&&[^_]]+(_)""")
}

protected case class EmphasisAsterixSplitter(val spanParser:SpanParser)
extends EmphasisSplitter {

    val start = new Regex("""(\*)[\S&&[^*]]+""")

    val end = new Regex("""[\S&&[^*]]+(\*)""")
}

protected trait StrongSplitter
extends MatchedSplitter {

    type Thing = Strong

    def construct(seq:Seq[Nad]) = Strong(seq)
}

protected case class StrongUnderscoreSplitter(val spanParser:SpanParser)
extends StrongSplitter {

    val start = new Regex("""(__)[\S&&[^_]]+""")

    val end = new Regex("""[\S&&[^_]]+(__)""")
}

protected case class StrongAsterixSplitter(val spanParser:SpanParser)
extends StrongSplitter {

    val start = new Regex("""(\*\*)[\S&&[^*]]+""")

    val end = new Regex("""[\S&&[^*]]+(\*\*)""")
}

protected object InlineHTMLSplitter {

    import util.matching._

    val starter = new Regex("""<[ ]*([a-zA-Z]+)[ ]*[\w="'&&[^>]]*[ ]*>""") // "'

    def split(str:String):List[Nad] = {
    
        starter.findFirstMatchIn(str) match {
            case Some(matchStart) => {
                val ender = new Regex("(?i)</[ ]*" + matchStart.group(1) + "[ ]*>")
                ender.findFirstMatchIn(matchStart.after) match  {
                    case Some(matchEnd) => {
                        val nads = new collection.mutable.Queue[Nad]
                        if (matchStart.before.length > 0)
                            nads += Text(matchStart.before.toString)
                        nads += HTML(str.substring(matchStart.start, matchStart.end + matchEnd.end).toString)
                        if (matchEnd.after.length > 0)
                            nads += Text(matchEnd.after.toString)
                        nads.toList
                    }
                    
                    case None => List(Text(str))
                }
            }
            case None => List(Text(str))
        }
    }
}

trait LinkSplitter {
 
    def starter:Regex
    
    type Thing <: Nad
    
    def construct(nads:List[Nad], url:String):Thing
    
    def construct(nads:List[Nad], url:String, title:String):Thing
    
    def spanParser:SpanParser
    
    def split(str:String):List[Nad] = {
        
        starter.findFirstMatchIn(str) match {
            case Some(mtch) => {

                val nads = new collection.mutable.Queue[Nad]

                if (mtch.before.length > 0)
                    nads += Text(mtch.before.toString)
                
                if (mtch.group(1) != null)
                    nads += construct(spanParser.parse(mtch.group(1)), mtch.group(2), mtch.group(3))
                else
                    nads += construct(spanParser.parse(mtch.group(4)), mtch.group(5))
                    
                if (mtch.after.length > 0)
                    nads += Text(mtch.after.toString)
                    
                nads.toList
            }
            case None => List(Text(str))
        }
    }
}

protected case class InlineLinkSplitter(val spanParser:SpanParser) extends LinkSplitter {
 
    import util.matching._
    
    type Thing = Link
    
    def construct(nads:List[Nad], url:String) = Link(nads, url)
    
    def construct(nads:List[Nad], url:String, title:String) = Link(nads, url, title)
    
    val starter = new Regex("""\[([^\]]*)\][ ]*\(([\S&&[^)]]*) "([^)]*)"\)|\[([^\]]*)\][ ]*\(([\S&&[^)]]*)\)""")
}


protected case class InlineImageLinkSplitter(val spanParser:SpanParser) extends LinkSplitter {
 
    import util.matching._
    
    type Thing = ImageLink
    
    def construct(nads:List[Nad], url:String) = ImageLink(nads, url)
    
    def construct(nads:List[Nad], url:String, title:String) = ImageLink(nads, url, title)
    
    val starter = new Regex("""!\[([^\]]*)\][ ]*\(([\S&&[^)]]*) "([^)]*)"\)|!\[([^\]]*)\][ ]*\(([\S&&[^)]]*)\)""")
}