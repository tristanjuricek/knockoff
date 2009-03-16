package com.tristanhunt.knockoff

import util.parsing.combinator._


/**
 * Ugh. Each span is first broken up by the HTML finder thingy. We then parse each subsection
 * as being not HTML with the span parser.
 */
class SpanParser
extends JavaTokenParsers {

}








object KnockOff {
 
    def parse(src:String):Option[List[Block]] = {
        
        // Replace tabs with 4 spaces.
        val src2 = src.replace("\t", "    ")
       
        val blockParser = new BlockParser
        val blocks = blockParser.parseAll(blockParser.markdownDocument, src2) match {

            case blockParser.Success(list, _) => Some(list).get

            case fail:blockParser.Failure => {
                println(fail.toString)
                return None
            }
            
            case _ => return None
        }
        
        // TODO condense any sequential lists.
        Some(_condense(blocks, Nil))
    }
    
    private def _condense(in:List[Block], out:List[Block]):List[Block] = {
        
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