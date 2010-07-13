# Part 2.A. Core Components #

# The Discounter #

The Discounter is a trait you can extend in order to customize things. Otherwise,
a "default" object is available at `DefaultDiscounter`.

    val blocks = DefaultDiscounter.knockoff( "document" )
    val xhtml = toXHTML( blocks ) // See the XHTML Writer

    // The Discounter
    trait Discounter extends ChunkStreamFactory with XHTMLWriter with TextWriter {
  
      /** Parses and returns our best guess at the sequence of blocks. It will
          never fail, just log all suspicious things. */
      def knockoff( source : java.lang.CharSequence ) : Seq[Block] = {
        
        val chunks = createChunkStream( new CharSequenceReader( source, 0 ) )
        
        // These next lines are really ugly because I couldn't figure out a nice
        // way to match a tuple argument (thank you erasure!)
        val linkDefinitions = chunks.flatMap{ case ((chunk, pos)) =>
          if ( chunk.isLinkDefinition )
            List( chunk.asInstanceOf[ LinkDefinitionChunk ] )
          else Nil
        }
        
        val convert = createSpanConverter( linkDefinitions )
        
        val spanned = chunks.map { chunkAndPos =>
          ( chunkAndPos._1, convert( chunkAndPos._1 ), chunkAndPos._2 )
        }
        
        combine( spanned.toList, new ListBuffer )
      }
      
      def createSpanConverter( linkDefinitions : Seq[LinkDefinitionChunk] ) : SpanConverter =
        new SpanConverter( linkDefinitions )
      
      /** Consume input and append the right thing to the output until empty. The
          Chunk itself determines the "right thing to do". All chunks only know what
          has come before itself, by peering into the output. (It shouldn't matter
          what comes next...) */
      private def combine( input : List[ (Chunk, Seq[Span], Position) ],
                           output : ListBuffer[Block] )
                         : Seq[ Block ] = {
        if ( input.isEmpty ) return output
        input.head._1.appendNewBlock( output, input.tail, input.head._2,
                                      input.head._3, this )
        combine( input.tail, output )
      }
    }

### The Default Discounter

Provides an object along with a main method for the "good enough to script" kind of
usage. Note that a major aim of this discounter is to mimic the usage of
`Markdown.pl`.

    Markdown.pl [ −−html4tags ] [ −−version ] [ −shortversion ] [ file ... ]

The `--html4tags` argument will just do nothing, but not be processed as a file.

    // The DefaultDiscounter
    object DefaultDiscounter extends Discounter with ConsoleLogger {
      def main( args : Array[ String ] ) : Unit = try {
        if ( args.contains("--version") ) {
          Console.err.print( "DefaultDiscounter " )
        }
        if ( args.contains("--version") || args.contains("-shortversion") ) {
          Console.err.println( "0.7.1-SNAPSHOT" )
          return 0
        }

        if ( args.isEmpty ) {
          val sb = new StringBuilder
          var line : String = null
          do {
            line = Console.readLine
            if ( line != null ) sb.append( line )
          } while ( line != null )
          println( toXHTML( knockoff( sb.toString ) ).toString )
        } else {
          args.filter( _ != "--html4tags" ).foreach { fileName =>
            println( toXHTML( knockoff( readText( fileName ) ) ).toString )
          }
        }
      } catch {
        case th : Throwable => {
          th.printStackTrace( Console.err )
        }
      }

      private def readText( fileName : String ) : String =
        io.Source.fromFile( new File( fileName ) ).mkString("")
    }

### `Discounter.scala`

    // In com/tristanhunt/knockoff/Discounter.scala
    package com.tristanhunt.knockoff
    
    import scala.collection.mutable.ListBuffer
    import scala.util.parsing.input.Position
    import scala.util.parsing.input.CharSequenceReader
    import scala.xml.{ Group, Node }
    
    // See the Discounter
    
    import java.io.File
    import scala.util.logging.ConsoleLogger
    
    // See the DefaultDiscounter



## The Object Model ##

The object model for Knockoff of course models the components of a Markdown
document. If you have to extend these, you will run into complexity issues with any
conversion format you want to extend into. Each new output format will need to take
care of those new types.

### Spanning Elements

Spanning elements either are strings that are simply tagged with specific meaning,
which make them simple, or they are composed of sequences of those strings.

Links can be direct or indirect, or tagged as image variations of those links.

    // The Knockoff Spanning Elements
    trait Span

    case class Text( content : String ) extends Span
    case class HTMLSpan( html : String ) extends Span
    case class CodeSpan( content : String ) extends Span
    
    case class Strong( children : Seq[Span] ) extends Span
    case class Emphasis( children : Seq[Span] ) extends Span

    case class Link( children : Seq[Span], url : String, title : Option[String] )
    extends Span

    case class IndirectLink( children : Seq[Span], definition : LinkDefinition )
    extends Span
    
    case class ImageLink( children : Seq[Span], url : String,
                          title : Option[String] )
    extends Span
    
    case class IndirectImageLink( children : Seq[Span],
                                  definition : LinkDefinition )
    extends Span

### Block Elements

Most of the block elements contain sequences of spanning elements, and importantly,
their parsing Position.

This position cannot currently be used to rebuild the actual source
document, just mark where we found the start of the block.

    // The Knockoff Block Elements
    trait Block { def position : Position }

    case class Paragraph( spans : Seq[Span], position : Position ) extends Block
    
    case class Header( level : Int, spans : Seq[Span], position : Position )
    extends Block
    
    case class LinkDefinition( id : String, url : String, title : Option[String],
                               position : Position )
    extends Block

    case class Blockquote( children : Seq[Block], position : Position )
    extends Block
    
    case class CodeBlock( text : Text, position : Position ) extends Block
    
    case class HorizontalRule( position : Position ) extends Block
    
    case class OrderedItem( children : Seq[Block], position : Position )
    extends Block
    
    case class UnorderedItem( children : Seq[Block], position : Position )
    extends Block
    
    case class OrderedList( items : Seq[OrderedItem] ) extends Block {
      lazy val position = if ( items.isEmpty ) NoPosition else items.first.position
    }
    
    case class UnorderedList( items : Seq[UnorderedItem] ) extends Block {
      lazy val position = if ( items.isEmpty ) NoPosition else items.first.position      
    }

### ObjectModel.scala

    // In com/tristanhunt/knockoff/ObjectModel.scala
    package com.tristanhunt.knockoff
    
    import scala.io.{ Source }
    import scala.util.parsing.input.{ NoPosition, Position }
    
    // See the Knockoff Spanning Elements
    
    // See the Knockoff Block Elements