package com.tristanhunt.knockoff


/**
 * A block, when complete, is just a list of spanning Nads. (God, I _love_ that name.)
 */
trait Block {
    def nads : List[ Nad ]
}

/**
 * Generally was not identified as a major block element of any other type, so this is likely the
 * most common element in your markdown document.
 */
case class Paragraph( val nads : List[ Nad ] )
extends Block

/**
 * Marks a heading element of some kind, which should be at a level between 1 and 6
 */
case class Header( val nads : List[ Nad ], val level : Int )
extends Block {

    assume( ( 1 <= level ) && ( level <= 6 ) )
}

/**
 * A blockquote contains other blocks, but we consider the other elements to be stored within 
 * `<blockquote>` tags.
 */
case class Blockquote( val blocks : List[ Block ] ) extends Block {

    val nads:List[ Nad ] = blocks.flatMap( _.nads ).toList
}

/**
 * We expect this really to be just a single HTML nad. Does implement the `Block` interface for 
 * completeness, however.
 */
case class HTMLBlock( val html : HTML )
extends Block {

    val nads = List(html)
}

/**
 * Marks the preformatted text block, stripped of any line leads, etc.
 */
case class CodeBlock(val preformatted:Text)
extends Block {

    val nads = List(preformatted)
}

case class HorizontalRule
extends Block {

    val nads = List(HorizontalRules.hr)
}

object HorizontalRules {

    /**
     * Mostly, this is just a simple, consistent string representation for what is a placeholder
     * for the HTML representation.
     */
    def hr = Text("***")
}

/**
 * A ordered or unordered sequence of blocks. Generally, you will only have Paragraphs, but it could
 * be HTML.
 */
trait BlockList extends Block {
    def items:Seq[Block]
    def nads:List[Nad] = items.flatMap(_.nads).toList
}

case class UnorderedBlockList(val items:Seq[Block])
extends BlockList

case class OrderedBlockList(val items:Seq[Block])
extends BlockList

trait ComplexBlockList extends Block {
    def items:Seq[ Seq[ Block ] ]
    def nads:List[Nad] = items.flatMap( blocks => blocks.flatMap(_.nads).toList ).toList
}

case class UnorderedComplexBlockList( val items : Seq[ Seq[ Block ] ] )
extends ComplexBlockList

case class OrderedComplexBlockList( val items : Seq[ Seq[ Block ] ] )
extends ComplexBlockList

/**
 * Each link definition should basically should match the ID case insensitively. 
 *
 * TODO Figure out how to conventiently call that comparator.
 */
case class LinkDefinition(val id:String, val url:String, val title:String)
