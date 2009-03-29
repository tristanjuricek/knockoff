package com.tristanhunt.knockoff

// TODO: These are intermediate representations. Almost everything will switch to some kind of 
// span-like mechanism in a second step.

/**
 * A nad is a markdown, er, node. You can get a list of them from a markdown document, which allow
 * you to then, like, mess around with them and shit.
 *
 * @author Tristan Juricek <mr.tristan@gmail.com>
 */
protected trait MkBlock {

    /**
     * The markdown chunk of this MkBlock.
     */
    def markdown:String
}

/**
 * Whitespace or whatever that is completely insignificant to the HTML generated.
 */
protected case class EmptySpace(val markdown:String) extends MkBlock

/**
 * An "other" type that's probably most of the actual markdown content.
 */
protected case class MkParagraph(val markdown:String) extends MkBlock

/**
 * A chunk of markdown tagged to being included as a header element.
 */
protected case class MkHeader(val markdown:String, val level:Int) extends MkBlock

/**
 * A blockquote is somewhat interesting, because it's basically a markdown element tree. So we
 * do reparse the blockquote content (once we know it) during translation. Note that quotes are not
 * however, embeddeable in each other.
 *
 * TODO What this means is that this damn blockquote actually has to be recursively parsed again
 * and again...
 */
protected case class MkBlockquote(val blockquote:String) extends MkBlock {
    
    def markdown:String = {
        val sb = new StringBuilder
        io.Source.fromString(blockquote).getLines.foreach(line => {
            val start = if (line.length > 1 && line.charAt(1) == ' ') 2 else 1
            sb.append(line.substring(start))
        })
        sb.toString
    }
}

/**
 * Inline HTML. Not really parsed, just a line that starts with a '<'
 *
 * NOTE: Contents are _not_ a URL.
 */
protected case class HTMLMkBlock(val markdown:String) extends MkBlock

/**
 * The sexy code block.
 */
protected case class CodeMkBlock(val markdown:String) extends MkBlock {
 
    def preformatted:String = {
        val sb = new StringBuilder
        io.Source.fromString(markdown).getLines.foreach(line => sb.append(line.substring(4)))
        sb.toString
    }
}

/**
 * Should be just replaced with some kind of horizontal rule in HTML, or border, whatever.
 */
protected case class MkHorizontalRule(val markdown:String) extends MkBlock

/**
 * Lists are just a sequence of the markdown blocks placed together. That is, we group all lines
 * of a single point together.
 */
protected trait MarkdownList {
    def items:Seq[String]
    def markdownSeq:Seq[String]
}

protected case class BulletListMkBlock(val items:Seq[String]) extends MarkdownList with MkBlock {
    
    val markdown:String = {
        val sb = new StringBuilder
        items.foreach(item => sb.append(item))
        sb.toString
    }
    
    /**
     * A version of items that removes the leading bullet marker.
     *
     * TODO Actual mapping
     */
    def markdownSeq:Seq[String] = items.map(item => {
        (new util.matching.Regex("""(?ms)^[*\-+]\s*(.*)$""")).findFirstMatchIn(item).get.group(1)
    })
}

protected case class NumberedListMkBlock(val items:Seq[String]) extends MarkdownList with MkBlock {
 
    val markdown:String = {
        val sb = new StringBuilder
        items.foreach(item => sb.append(item))
        sb.toString
    }
    
    def markdownSeq:Seq[String] = items.map(item => {
        (new util.matching.Regex("""(?ms)^\d\.\s+(.*)$""")).findFirstMatchIn(item).get.group(1)
    })
}

/**
 * Should only be used for links referenced elsewhere.
 *
 * Variations:
 *
 *     [ID]: http://example.com/ "Optional Title"
 *     [ID]: http://example.com/ 'Optional Title'
 *     [ID]: http://example.com/ (Optional Title)
 * 
 * Multiline:
 *     [ID]: http://example.com/
 *           "Title Here"
 */
case class MkLinkDefinition(id:String, url:String, title:String) extends MkBlock {
    
    val markdown:String = String.format("""[%s]: %s '%s'""", id, url, title)
}

case class MkLinkDefinitionList(definitions:List[MkLinkDefinition]) extends MkBlock {
 
    val markdown:String = {
        val sb = new StringBuilder
        definitions.foreach(d => sb.append(d.markdown).append("\n"))
        sb.toString
    }
}