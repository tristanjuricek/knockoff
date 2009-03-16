package com.tristanhunt.knockoff

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
case class EmptySpace(val markdown:String) extends Block

case class TextBlock(val markdown:String) extends Block {
    
}

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