# `Discounter` - The One Trait #

You inherit from `KnockOff` to be able to parse your sources. If you're not doing
fancy customization, you use the DefaultDiscounter.

    val blocks = DefaultDiscounter.knockoff( "document" )

    println( "Hello, Conversion!" )
    println( blocks.toXML )

Otherwise...

    // In knockoff2/Discounter.scala
    // See the Discounter package and imports
    
    trait   Discounter
    extends ChunkStreamFactory
    with    SpanConverterFactory {
    
        /**
         * Parses and returns our best guess at the sequence of blocks. It will
         * never fail, just log all suspicious things.
         */
        def knockoff( source : java.lang.CharSequence ) : BlockSeq = {
            
            val chunks = createChunkStream( new CharSequenceReader( source, 0 ) )

            val linkDefinitions = chunks.flatMap( chunk => chunk match {
                case ld : LinkDefinition => List( ld )
                case _ => Nil
            } )
            
            val convert = spanConverter( linkDefinitions )
            
            val spanned = chunks.map { chunkAndPos =>
                ( chunkAndPos._1, convert( chunkAndPos._1 ), chunkAndPos._2 )
            }
            
            recombine( spanned )
        }
        
        /**
         * Part of recombination is the fancy bit.
         */
        def recombine( spannedChunks : Seq[ (Chunk, SpanSeq, Position) ] ) : BlockSeq = {
            error( "not implemented" )
        }
    }

#### Package And Imports

    // The Discounter package and imports
    package knockoff2
    
    import scala.util.parsing.input.Position
    import scala.util.parsing.input.CharSequenceReader

### `DefaultDiscounter` ###

    // In knockoff2/DefaultDiscounter.scala
    package knockoff2
    
    import scala.util.logging.ConsoleLogger
    
    object DefaultDiscounter extends Discounter with ConsoleLogger