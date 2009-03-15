package com.tristanhunt.knockoff

import util.parsing.combinator.JavaTokenParsers

/**
 * A nad is a markdown, er, node. You can get a list of them from a markdown document, which allow
 * you to then, like, mess around with them and shit.
 *
 * @author Tristan Juricek <juricek@emarsys.com>
 */
trait Block {

    /**
     * The markdown chunk of this Block.
     */
    def markdown:String    
}

/**
 * Whitespace or whatever that is completely insignificant to the HTML generated.
 */
case class EmptySpace(
    val markdown:String)
extends Block {
}
case class TextBlock(val markdown:String) extends Block

case class Header(val markdown:String, val level:Int) extends Block

/**
 * A blockquote is somewhat interesting, because it's basically a markdown element tree. So we
 * do reparse the blockquote content (once we know it) during translation.
 */
case class Blockquote(val markdown:String) extends Block

/**
 * Inline HTML. Not really parsed, just a line that starts with a '<'
 */
case class HTMLBlock(val markdown:String) extends Block

/**
 * The sexy code block.
 */
case class CodeBlock(val markdown:String) extends Block

case class HorizontalRule(val markdown:String) extends Block

trait MarkdownList {
    def items:Seq[String]
}

case class BulletListBlock(val items:Seq[String]) extends MarkdownList with Block {
    
    val markdown:String = {
        val sb = new StringBuilder
        items.foreach(item => sb.append(item))
        sb.toString
    }
}

case class NumberedListBlock(val items:Seq[String]) extends MarkdownList with Block {
 
    val markdown:String = {
        val sb = new StringBuilder
        items.foreach(item => sb.append(item))
        sb.toString
    }
}


/**
 * Should be used at the start to identify block elements. We use empty lines to identify block
 * elements solely. (Thus, the rule to allow list items to also appear in the same list is applied
 * elsewhere in a second pass.)
 *
 * @author Tristan Juricek <juricek@emarsys.com>
 */
class BlockParser
extends JavaTokenParsers {
    
    /**
     * Whitespace does make grouping significant in this system. This class should deal with all
     * significant whitespace, however, and pass it through properly.
     */
    override def skipWhitespace = false
    
    /**
     * A markdown document
     */
    def markdownDocument:Parser[List[Block]]  = repsep(thing, emptyLine)
    
    def thing:Parser[Block] =
        (codeBlock | horizontalRule | listBlock | header | htmlBlock | blockquote | textBlock) ^^
            (d => d.asInstanceOf[Block])

    /**
     * Text lines need to retain their newlines. The user can use "hard" formatting with newlines,
     * and also indicate two spaces at the end of the line to force a paragraph break.
     */
    def textLine:Parser[TextBlock]      = """[ ]*\S[^\n]*\n?""".r   ^^ (str => TextBlock(str))
    def textBlock:Parser[TextBlock]     = rep(textLine)     ^^ (list => TextBlock(_concatTextBlocks(list)))
    
    /**
     * We don't actually parse HTML, just assume the simple rule of "if it looks like markup, you
     * intended it to be markup". At least until there's some blank space.
     */
    def htmlBlock:Parser[HTMLBlock]     = htmlLine~rep(textLine)    ^^ (p => HTMLBlock(_concatTextBlocks(p._1 :: p._2)))
    def htmlLine:Parser[TextBlock]      = """<.*\n?""".r            ^^ (str => TextBlock(str))

    /**
     * For some reason, this groups bullet lists broken with an empty line into a single list...
     */
    def listBlock:Parser[Block] = bulletList | numberedList

    def bulletList:Parser[BulletListBlock] =
        rep1(bulletItem) ^^ (list => BulletListBlock(list))

    def bulletItem:Parser[String] =
        bulletLead~rep(noBulletLine) ^^ (p => p._1 + _concatTextBlocks(p._2))

    def bulletLead:Parser[String]           = """[*\-+]""".r~textLine               ^^ (l => l._1 + l._2.markdown)
    def noBulletLine:Parser[TextBlock]      = """[ ]*[\S&&[^*\-+]][^\n]*\n?""".r    ^^ (s => TextBlock(s))
    
    
    def numberedList:Parser[NumberedListBlock] =
        rep1(numberedItem) ^^ (list => NumberedListBlock(list))

    def numberedItem:Parser[String] =
        numberedLead~rep(noNumberLine) ^^ (p => p._1 + _concatTextBlocks(p._2))
        
    def numberedLead:Parser[String] =
        """\d+\.""".r~textLine ^^ (pair => pair._1 + pair._2.markdown)
        
    def noNumberLine:Parser[TextBlock] = not(numberedLead)~>textLine
    
    /**
     * One or more blank lines mostly is used to break blocks apart from each other in the list.
     * This should be generally passed over in blocks. Note that this should generally capture
     * more lines as a single empty space element.
     */
    def emptyLine:Parser[EmptySpace]    = """\s*\n""".r     ^^ (s => EmptySpace(s))
    
    def header:Parser[Header]           = (setextHeader1 | setextHeader2 | atxHeader)
    def setextHeader1:Parser[Header]    = textLine<~equalsLine  ^^ (md => Header(md.markdown.trim, 1))
    def setextHeader2:Parser[Header]    = textLine<~dashLine    ^^ (md => Header(md.markdown.trim, 2))
    def equalsLine:Parser[Any]          = """=+\n?""".r
    def dashLine:Parser[Any]            = """-+\n?""".r
    def atxHeader:Parser[Header]        = """#+ .*\n?""".r       ^^ (str => _atxHeader(str.trim))
    
    
    def blockquote:Parser[Blockquote] =
        rep1(blockquotedLine) ^^ (list => Blockquote(_concatTextBlocks(list)))
    
    def blockquotedLine:Parser[TextBlock] =
        ">"~(textLine | emptyLine) ^^ (tup => TextBlock(tup._1 + tup._2.markdown))
        
    def codeBlock:Parser[CodeBlock] =
        rep1(indentedLine) ^^ (list => CodeBlock(_concatTextBlocks(list)))
        
    def indentedLine:Parser[TextBlock] =
        "    "~(textLine | emptyLine) ^^ (tup => TextBlock(tup._1 + tup._2.markdown))
        
    def horizontalRule:Parser[HorizontalRule] =
        """[*\-_][ ]?[*\-_][ ]?[*\-_][ *\-_]*\n?""".r ^^ (rule => HorizontalRule(rule))
    
    /**
     * Combine hard-wrapped lines together into single blocks.
     */
    private def _concatTextBlocks(list:List[TextBlock]):String = {
        val sb = new StringBuilder
        list.foreach(tb => sb.append(tb.markdown))
        sb.toString
    }
    
    /**
     * Remove any leading and trailing # characters, but use the leading # character to determine 
     * level of the header.
     */
    private def _atxHeader(txt:String):Header = {
        import util.matching.Regex
        val r = new Regex("""^(#+) (.*?)\s?#*+$""")
        Header(r.replaceFirstIn(txt, "$2").trim, r.replaceFirstIn(txt, "$1").length)
    }
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