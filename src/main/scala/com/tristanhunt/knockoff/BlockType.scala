package com.tristanhunt.knockoff

/**
 * Used to indicate the type of blocks you are interested in when filtering via
 * the BlockSeq.filterType method.
 */
trait BlockType[T <: Block] { def wrappedClass : Class[ T ] }

case object Paragraphs
extends BlockType[ Paragraph ] { def wrappedClass = classOf[ Paragraph ] }

case object Headers
extends BlockType[ Header ] { def wrappedClass = classOf[ Header ] }

case object LinkDefinitions
extends BlockType[ LinkDefinition ] {
  def wrappedClass = classOf[ LinkDefinition ]
}

case object Blockquotes
extends BlockType[ Blockquote ] { def wrappedClass = classOf[ Blockquote ] }

case object CodeBlocks
extends BlockType[ CodeBlock ] { def wrappedClass = classOf[ CodeBlock ] }

case object HorizontalRules
extends BlockType[ HorizontalRule ] {
  def wrappedClass = classOf[ HorizontalRule ]
}

case object OrderedItems
extends BlockType[ OrderedItem ] { def wrappedClass = classOf[ OrderedItem ] }

case object UnorderedItems
extends BlockType[ UnorderedItem ] {
  def wrappedClass = classOf[ UnorderedItem ]
}

case object MarkdownLists
extends BlockType[ MarkdownList ] { def wrappedClass = classOf[ MarkdownList ] }
