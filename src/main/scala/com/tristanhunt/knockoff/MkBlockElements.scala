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
    
    /**
     * In the case of multiline indents, this will return the block without the extra four spaces.
     */
    def unindentedClone:MkBlock = clone( markdown.replace( "\n    ", "\n" ) )
    
    def clone( markdown : String ):MkBlock = error( "should not be called" )
}

/**
 * Whitespace or whatever that is completely insignificant to the HTML generated.
 */
protected case class EmptySpace(val markdown:String) extends MkBlock {
 
    override def clone( markdown : String ) = EmptySpace( markdown )
}

/**
 * An "other" type that's probably most of the actual markdown content.
 */
protected case class MkParagraph(val markdown:String) extends MkBlock {
    
    override def clone( markdown : String ) = MkParagraph( markdown )
}

/**
 * A chunk of markdown tagged to being included as a header element.
 */
protected case class MkHeader(val markdown:String, val level:Int) extends MkBlock {
 
    override def clone( markdown : String ) = MkHeader( markdown, level )
}

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
protected case class HTMLMkBlock(val markdown:String) extends MkBlock {
    
    override def clone( markdown : String ) = HTMLMkBlock( markdown )
}

/**
 * The sexy code block.
 */
protected case class CodeMkBlock(val markdown:String) extends MkBlock {
 
    def preformatted:String = {
        val sb = new StringBuilder
        io.Source.fromString(markdown).getLines.foreach(line => sb.append(line.substring(4)))
        sb.toString
    }
    
    override def clone( markdown : String ) = CodeMkBlock( markdown )
}

/**
 * Should be just replaced with some kind of horizontal rule in HTML, or border, whatever.
 */
protected case class MkHorizontalRule(val markdown:String) extends MkBlock {
 
    override def clone( markdown : String ) = MkHorizontalRule( markdown )
}

/**
 * Lists are just a sequence of the markdown blocks placed together. That is, we group all lines
 * of a single point together, and each of those will be considered a single paragraph of 
 * information.
 */
protected trait MarkdownList extends MkBlock {

    def items : Seq[ String ]
    
    def markdown : String = error( "You shouldn't fetch the markdown of a MarkdownList" )
    
    override def unindentedClone:MkBlock = {
     
        val itemList : List[ String ] = items.toList
        
        val tailList : List[ String ] = itemList.tail.map( item => item.replace( "\n    ", "\n" ) )
        
        clone( itemList.head :: tailList )
    }
    
    def clone( items : Seq[ String ] ):MarkdownList = error( "oops, should be overridden" )
}

/**
 * This captures the "simple bulleted list", which is a list where each group of items can be
 * treated as a single paragraph.
 */
protected case class BulletListMkBlock( val items:Seq[ String ] )
extends MarkdownList with MkBlock {
 
    override def clone( items : Seq[ String ] ) = BulletListMkBlock( items )
}

/**
 * The simpler numbered list, where each item will become it's own paragraph.
 */
protected case class NumberedListMkBlock( val items : Seq[ String ] )
extends MarkdownList with MkBlock {
    
    override def clone( items : Seq[ String ] ) = NumberedListMkBlock( items )
}

/**
 * Lists are just a sequence of the markdown blocks placed together. That is, we group all lines
 * of a single point together.
 */
protected trait ComplexMarkdownList extends MkBlock {

    def items:Seq[ Seq[ MkBlock ] ]

    def markdown:String = {
        error( "Should not fetch the markdown of a list item." )
    }
    
    override def unindentedClone : MkBlock = {
     
        val unindented = items.map( blockSeq => {
            
            val blockList = blockSeq.toList
            
            blockList.head :: blockList.tail.map( item => item.unindentedClone )
        } )
        
        clone( unindented )
    }
    
    def clone( items : Seq[ Seq[ MkBlock ] ] ):ComplexMarkdownList = error( "Should be overridden" )
}

/**
 * The complex bullet list is a sequence of sequences of other blocks.
 */
protected case class ComplexBulletListMkBlock(
    val items:Seq[ Seq[ MkBlock ] ]
)
extends ComplexMarkdownList with MkBlock {
    
    override def clone( items : Seq[ Seq[ MkBlock ] ] ) = ComplexBulletListMkBlock( items )
}

/**
 * The complex bullet list is a sequence of sequences of other blocks.
 */
protected case class ComplexNumberedListMkBlock(
    val items:Seq[ Seq[ MkBlock ] ]
)
extends ComplexMarkdownList with MkBlock {
 
    override def clone( items : Seq[ Seq[ MkBlock ] ] ) = ComplexNumberedListMkBlock( items )
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