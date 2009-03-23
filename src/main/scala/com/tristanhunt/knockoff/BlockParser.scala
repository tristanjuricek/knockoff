package com.tristanhunt.knockoff

import util.parsing.combinator._

/**
 * Should be used at the start to identify block elements. We use empty lines to identify block
 * elements solely. (Thus, the rule to allow list items to also appear in the same list is applied
 * elsewhere in a second pass.)
 *
 * TODO: Need to add the link reference line, and, the current BlockElements are actually 
 * intermediate stages. Meh.
 *
 * @author Tristan Juricek <juricek@emarsys.com>
 */
class BlockParser
extends RegexParsers {
    
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
        (codeBlock | horizontalRule | listBlock | header | linkDefinition | htmlBlock |
            blockquote | textBlock) ^^ (d => d.asInstanceOf[Block])

    /**
     * Text lines need to retain their newlines. The user can use "hard" formatting with newlines,
     * and also indicate two spaces at the end of the line to force a paragraph break.
     */
    def textLine:Parser[TextBlock]      = """[ ]*\S[^\n]*\n?""".r   ^^ (str => TextBlock(str))
    def textBlock:Parser[TextBlock]     = rep(textLine)     ^^ (list => TextBlock(_concatTextBlocks(list)))
    
    /**
     * We don't actually parse HTML, just assume the simple rule of "if it looks like markup, you
     * intended it to be markup". At least until there's some blank space.
     *
     * NOTE: We need to avoid the <http://fancy.link> automatic link interface here.
     */
    def htmlBlock:Parser[HTMLBlock]     = htmlLine~rep(textLine)    ^^ (p => HTMLBlock(_concatTextBlocks(p._1 :: p._2)))
    def htmlLine:Parser[TextBlock]      = """<[ ]*[\w="]*[ ]*\n?""".r            ^^ (str => TextBlock(str))
    
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
        "    "~>(textLine | emptyLine) ^^ (v => TextBlock(v.markdown))
        
    def horizontalRule:Parser[HorizontalRule] =
        """[*\-_][ ]?[*\-_][ ]?[*\-_][ *\-_]*\n?""".r ^^ (rule => HorizontalRule(rule))
    
    def linkDefinition:Parser[LinkDefinitionList] = rep1(linkDefinitionItem) ^^ (list => LinkDefinitionList(list))
    
    def linkDefinitionItem:Parser[LinkDefinition] = linkBase~opt(linkTitle)<~"""[ ]*\n""".r ^^ (
        parser => parser._2 match {
            case Some(title)    => LinkDefinition(parser._1._1, parser._1._2, title)
            case None           => LinkDefinition(parser._1._1, parser._1._2, "")
        })
            
    def linkBase:Parser[(String,String)] =
        """[ ]{0,3}\[[^\[\]]*\][ ]+[\w\p{Punct}]+""".r ^^ (str => _readLinkBase(str))
    
    def linkTitle:Parser[String] = """\s*""".r~>"""["'(].*["')]""".r ^^
        (str => str.substring(1, str.length - 1))

    /**
     * Cheap way of grouping the linkBase regex result. I find this to be an ugly solution.
     */
    private def _readLinkBase(str:String):(String,String) = {
        val regex = new util.matching.Regex("""^\[([^\[\]]+)\][ ]+([\w\p{Punct}]+)$""")
        val mtch = regex.findFirstMatchIn(str.trim).get
        (mtch.group(1), mtch.group(2))
    }
    
    
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