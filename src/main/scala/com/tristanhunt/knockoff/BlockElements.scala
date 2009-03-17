package com.tristanhunt.knockoff

/**
 * A nad is a markdown, er, node. You can get a list of them from a markdown document, which allow
 * you to then, like, mess around with them and shit.
 *
 * @author Tristan Juricek <mr.tristan@gmail.com>
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

/**
 * An "other" type that's probably most of the actual markdown content.
 */
case class TextBlock(val markdown:String) extends Block

/**
 * A chunk of markdown tagged to being included as a header element.
 */
case class Header(val markdown:String, val level:Int) extends Block

/**
 * A blockquote is somewhat interesting, because it's basically a markdown element tree. So we
 * do reparse the blockquote content (once we know it) during translation. Note that quotes are not
 * however, embeddeable in each other.
 */
case class Blockquote(val blockquote:String) extends Block {
    
    /**
     * TODO: Translate the blockquote string to remove the leading '>' character.
     */
    def markdown:String = blockquote
}

/**
 * Inline HTML. Not really parsed, just a line that starts with a '<'
 */
case class HTMLBlock(val markdown:String) extends Block

/**
 * The sexy code block.
 */
case class CodeBlock(val markdown:String) extends Block

/**
 * Should be just replaced with some kind of horizontal rule in HTML, or border, whatever.
 */
case class HorizontalRule(val markdown:String) extends Block

/**
 * Lists are just a sequence of the markdown blocks placed together. That is, we group all lines
 * of a single point together.
 */
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