package knockoff2

/**
 * Used to indicate the type of blocks you are interested in when filtering via
 * the BlockSeq.? method.
 */
trait BlockType[T <: Block] { def wrappedClass : Class[ T ] }

case object Paragraphs
extends BlockType[ Paragraph ] { def wrappedClass = classOf[ Paragraph ] }

case object Headers
extends BlockType[ Header ] { def wrappedClass = classOf[ Header ] }
