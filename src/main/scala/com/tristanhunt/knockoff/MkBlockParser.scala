package com.tristanhunt.knockoff

import util.parsing.combinator._

/**
 * Parses a complete markdown source into intermediate 'MkBlock' elements. Basically, identifies
 * all of the "block" elements before we parse into spanning elements. 
 *
 * @author Tristan Juricek <juricek@emarsys.com>
 */
class MkBlockParser
extends RegexParsers {
    
    /**
     * Whitespace does make grouping significant in this system. This class should deal with all
     * significant whitespace, however, and pass it through properly.
     */
    override def skipWhitespace = false
    
    /**
     * A markdown document
     */
    def markdownDocument:Parser[List[MkBlock]]  = repsep(thing, emptyLine)
    
    def thing:Parser[MkBlock] =
        (codeMkBlock | horizontalRule | listMkBlock | header | linkDefinition | htmlMkBlock |
            blockquote | textMkBlock) ^^ (d => d.asInstanceOf[MkBlock])

    /**
     * Text lines need to retain their newlines. The user can use "hard" formatting with newlines,
     * and also indicate two spaces at the end of the line to force a paragraph break.
     */
    def textLine:Parser[MkParagraph]      = """[ ]*\S[^\n]*\n?""".r   ^^ (str => MkParagraph(str))
    def textMkBlock:Parser[MkParagraph]     = rep(textLine)     ^^ (list => MkParagraph(_concatMkParagraphs(list)))
    
    /**
     * We don't actually parse HTML, just assume the simple rule of "if it looks like markup, you
     * intended it to be markup". At least until there's some blank space.
     *
     * NOTE: We need to avoid the <http://fancy.link> automatic link interface here.
     */
    def htmlMkBlock:Parser[HTMLMkBlock]     = htmlLine~rep(textLine)    ^^ (p => HTMLMkBlock(_concatMkParagraphs(p._1 :: p._2)))
    def htmlLine:Parser[MkParagraph]      = """<[ ]*[\w="]*[ ]*\n?""".r            ^^ (str => MkParagraph(str))
    
    /**
     * For some reason, this groups bullet lists broken with an empty line into a single list...
     */
    def listMkBlock:Parser[MkBlock] = bulletList | numberedList

    def bulletList:Parser[BulletListMkBlock] =
        rep1(bulletItem) ^^ (list => BulletListMkBlock(list))

    def bulletItem:Parser[String] =
        bulletLead~rep(noBulletLine) ^^ (p => p._1 + _concatMkParagraphs(p._2))

    def bulletLead:Parser[String]           = """[*\-+]""".r~textLine               ^^ (l => l._1 + l._2.markdown)
    def noBulletLine:Parser[MkParagraph]      = """[ ]*[\S&&[^*\-+]][^\n]*\n?""".r    ^^ (s => MkParagraph(s))
    
    
    def numberedList:Parser[NumberedListMkBlock] =
        rep1(numberedItem) ^^ (list => NumberedListMkBlock(list))

    def numberedItem:Parser[String] =
        numberedLead~rep(noNumberLine) ^^ (p => p._1 + _concatMkParagraphs(p._2))
        
    def numberedLead:Parser[String] =
        """\d+\.""".r~textLine ^^ (pair => pair._1 + pair._2.markdown)
        
    def noNumberLine:Parser[MkParagraph] = not(numberedLead)~>textLine
    
    /**
     * One or more blank lines mostly is used to break blocks apart from each other in the list.
     * This should be generally passed over in blocks. Note that this should generally capture
     * more lines as a single empty space element.
     */
    def emptyLine:Parser[EmptySpace]    = """\s*\n""".r     ^^ (s => EmptySpace(s))
    
    def header:Parser[MkHeader]           = (setextHeader1 | setextHeader2 | atxHeader)
    def setextHeader1:Parser[MkHeader]    = textLine<~equalsLine  ^^ (md => MkHeader(md.markdown.trim, 1))
    def setextHeader2:Parser[MkHeader]    = textLine<~dashLine    ^^ (md => MkHeader(md.markdown.trim, 2))
    def equalsLine:Parser[Any]          = """=+\n?""".r
    def dashLine:Parser[Any]            = """-+\n?""".r
    def atxHeader:Parser[MkHeader]        = """#+ .*\n?""".r       ^^ (str => _atxHeader(str.trim))
    
    
    def blockquote:Parser[MkBlockquote] =
        rep1(blockquotedLine) ^^ (list => MkBlockquote(_concatMkParagraphs(list)))
    
    def blockquotedLine:Parser[MkParagraph] =
        ">"~(textLine | emptyLine) ^^ (tup => MkParagraph(tup._1 + tup._2.markdown))
        
    def codeMkBlock:Parser[CodeMkBlock] =
        rep1(indentedLine) ^^ (list => CodeMkBlock(_concatMkParagraphs(list)))
        
    def indentedLine:Parser[MkParagraph] =
        "    "~(textLine | emptyLine) ^^ (v => MkParagraph(v._1 + v._2.markdown))
        
    def horizontalRule:Parser[MkHorizontalRule] =
        """[*\-_][ ]?[*\-_][ ]?[*\-_][ *\-_]*\n?""".r ^^ (rule => MkHorizontalRule(rule))
    
    def linkDefinition:Parser[MkLinkDefinitionList] = rep1(linkDefinitionItem) ^^ (list => MkLinkDefinitionList(list))
    
    def linkDefinitionItem:Parser[MkLinkDefinition] = linkBase~opt(linkTitle)<~"""[ ]*\n?""".r ^^ (
        parser => parser._2 match {
            case Some(title)    => MkLinkDefinition(parser._1._1, parser._1._2, title)
            case None           => MkLinkDefinition(parser._1._1, parser._1._2, "")
        })
            
    def linkBase:Parser[(String,String)] =
        """[ ]{0,3}\[[^\[\]]*\]:[ ]+[\w\p{Punct}]+""".r ^^ (str => _readLinkBase(str))
    
    def linkTitle:Parser[String] = """\s*""".r~>"""["'(].*["')]""".r ^^
        (str => str.substring(1, str.length - 1))

    /**
     * Cheap way of grouping the linkBase regex result. I find this to be an ugly solution.
     */
    private def _readLinkBase(str:String):(String,String) = {
        val regex = new util.matching.Regex("""^\[([^\[\]]+)\]:[ ]+([\w\p{Punct}]+)$""")
        val mtch = regex.findFirstMatchIn(str.trim).get
        (mtch.group(1), mtch.group(2))
    }
    
    
    /**
     * Combine hard-wrapped lines together into single blocks.
     */
    private def _concatMkParagraphs(list:List[MkParagraph]):String = {
        val sb = new StringBuilder
        list.foreach(tb => sb.append(tb.markdown))
        sb.toString
    }
    
    /**
     * Remove any leading and trailing # characters, but use the leading # character to determine 
     * level of the header.
     */
    private def _atxHeader(txt:String):MkHeader = {
        import util.matching.Regex
        val r = new Regex("""^(#+) (.*?)\s?#*+$""")
        MkHeader(r.replaceFirstIn(txt, "$2").trim, r.replaceFirstIn(txt, "$1").length)
    }
}