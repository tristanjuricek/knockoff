package knockoff2

trait Chunk { def content : String }

/** Mostly, a Chunk that is not empty. */
case class TextChunk( val content : String ) extends Chunk

/** Note that this does not cover forced line breaks. */
case class EmptySpace( val content : String ) extends Chunk

case class BulletLineChunk( val content : String ) extends Chunk

case class NumberedLineChunk( val content : String ) extends Chunk
