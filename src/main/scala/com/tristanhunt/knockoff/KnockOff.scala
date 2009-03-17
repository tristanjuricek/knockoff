package com.tristanhunt.knockoff

import util.parsing.combinator._


/**
 * Ugh. Each span is first broken up by the HTML finder thingy. We then parse each subsection
 * as being not HTML with the span parser.
 */
class SpanParser
extends JavaTokenParsers {

}

/**
 * A Markdown-like parser implementation that returns an object model capable of being easily 
 * translated into, well, some kind of markup document.
 *
 * @author Tristan Juricek <mr.tristan@gmail.com>
 */
object KnockOff {
 
    /**
     * Return
     */
    def parse(src:String):Option[List[Block]] = {
        
        // Replace tabs with 4 spaces.
        val src2 = src.replace("\t", "    ")
       
        val blockParser = new BlockParser
        blockParser.parseAll(blockParser.markdownDocument, src2) match {

            case blockParser.Success(list, _) => Some(_condenseSpacedLists(blocks, Nil))

            case fail:blockParser.Failure => {
                println(fail.toString)
                return None
            }
            
            case _ => return None
        }
    }
    
    /**
     * Special rule to group any lists that were separated by whitespace, because it is an option.
     */
    private def _condenseSpacedLists(in:List[Block], out:List[Block]):List[Block] = {
        
        if (in.isEmpty) {
            return out
        }
        
        if (in.head.isInstanceOf[MarkdownList] && in.head.getClass == out.last.getClass) {
            val condensed = out.last match {
                case BulletListBlock(outItems)  => BulletListBlock(outItems ++ in.head.asInstanceOf[MarkdownList].items)
                case NumberedListBlock(outItems)        => NumberedListBlock(outItems ++ in.head.asInstanceOf[MarkdownList].items)
                case _ => error("Unknown MarkdownList Class: " + out.head.getClass)
            }
            return _condense(in.tail, out.dropRight(1) + condensed)
        }
        
        return _condense(in.tail, out + in.head)
    }
}