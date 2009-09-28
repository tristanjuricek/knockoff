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
    with    SpanConverterFactory
    with    HasElementFactory {
  
      /**
        Parses and returns our best guess at the sequence of blocks. It will
        never fail, just log all suspicious things.
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
        
        combine( spanned.toList, new ListBuffer )
      }
      
      /**
        Recursively combine the next element of the input into the output sequence.
        Because some elements (lists, code blocks separated uncleanly) affect the
        output of nearby elements, this can alter how the next element is converted.
        
        @param input The recognized element being operated on
        @param output The output sequence built in order.
      */
      private def combine(
        input   : List[ (Chunk, SpanSeq, Position) ],
        output  : ListBuffer[ Block ]
      ) : BlockSeq = {
        if ( input.isEmpty ) return new GroupBlock( output.toSeq )

        val factory = elementFactory
        import factory._

        input.firstOption.foreach{ case ((chunk, spans, position)) =>
          chunk match {
            case TextChunk(_) =>
              output += para( toSpan(spans), position )
            case BulletLineChunk(_) => {
              val li = usi( toSpan(spans), position )
              output.last match {
                case ul : UnorderedList => {
                  val appended = ul + li
                  output.update( output.length - 1, appended )
                }
                case _ => output += simpleUL( li )
              }
            }
            case NumberedLineChunk(_) => {
              val li = osi( toSpan(spans), position )
              output.last match {
                case ol : OrderedList => {
                  val appended = ol + li
                  output.update( output.length - 1, appended )
                }
                case _ => output += simpleOL( li )
              }
            }
            case EmptySpace(_) => {}
          }
        }
        
        combine( input.tail, output )
      }
    }

#### Package And Imports

    // The Discounter package and imports
    package knockoff2
    
    import scala.collection.mutable.ListBuffer
    import scala.util.parsing.input.Position
    import scala.util.parsing.input.CharSequenceReader

### `DefaultDiscounter` ###

For many applications the default is "good enough". Note that a major aim of this
discounter is to mimic the usage of `Markdown.pl`.

    Markdown.pl [ −−html4tags ] [ −−version ] [ −shortversion ] [ file ... ]

The `--html4tags` argument will just do nothing, but not be processed as a file.

    // In knockoff2/DefaultDiscounter.scala
    package knockoff2
    
    import scala.util.logging.ConsoleLogger
    
    object DefaultDiscounter extends Discounter with ColoredLogger {
      def main( args : Array[ String ] ) : Unit = try {
        if ( args.contains("--version") ) {
          Console.err.print( "DefaultDiscounter " )
        }
        if ( args.contains("--version") || args.contains("-shortversion") ) {
          Console.err.println( "0.5.0-SNAPSHOT" )
          return 0
        }
        
        if ( args.isEmpty ) {
          val sb = new StringBuilder
          var line : String = null
          do {
            line = Console.readLine
            if ( line != null ) sb.append( line )
          } while ( line != null )
          println( knockoff( sb.toString ).toXML.toString )
        } else {
          args.filter( _ != "--html4tags" ).foreach { fileName =>
            val group = knockoff( readText( fileName ) )
            println( knockoff( readText( fileName ) ).toXML.toString )
          }
        }
      } catch {
        case th : Throwable => {
          th.printStackTrace( Console.err )
        }
      }
      
      private def readText( fileName : String ) : String =
        io.Source.fromFile( fileName ).mkString("")
    }
