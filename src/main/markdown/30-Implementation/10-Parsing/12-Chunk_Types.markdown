# `Chunk` Types #

This is more of a reference to the typing of chunks.


## `Chunk` ##

    // In knockoff2/Chunk.scala
    package knockoff2
    
    trait Chunk { def content : String }


## `TextChunk` ##

    // In knockoff2/TextChunk.scala
    package knockoff2
    
    /** Mostly, a Chunk that is not empty. */
    case class TextChunk( val content : String ) extends Chunk


## `EmptySpace` ##

Multiple empty lines are tagged as `EmptySpace`. Note that this does *not* cover
forced line breaks.

    // In knockoff2/EmptySpace.scala
    package knockoff2
    
    case class EmptySpace( val content : String ) extends Chunk

