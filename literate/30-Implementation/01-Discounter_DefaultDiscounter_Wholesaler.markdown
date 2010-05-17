# `Discounter` and the `Wholesaler` #

You inherit from `KnockOff` to be able to parse your sources. If you're not doing
fancy customization, you can use the DefaultDiscounter.

    val blocks = DefaultDiscounter.knockoff( "document" )

    println( "Hello, Conversion!" )
    println( blocks.toXML )

Otherwise...

    // In com/tristanhunt/knockoff/Discounter.scala
    package com.tristanhunt.knockoff
    // See the Discounter imports
    
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
        
        val convert = new SpanConverter( linkDefinitions )
        
        val spanned = chunks.map { chunkAndPos =>
          ( chunkAndPos._1, convert( chunkAndPos._1 ), chunkAndPos._2 )
        }
        
        combine( spanned.toList, new ListBuffer )
      }
      
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
    
    // The Discounter imports
    
    import scala.collection.mutable.ListBuffer
    import scala.util.parsing.input.Position
    import scala.util.parsing.input.CharSequenceReader
    import scala.xml.{ Group, Node }


### `DefaultDiscounter` ###

For many applications the default is "good enough". Note that a major aim of this
discounter is to mimic the usage of `Markdown.pl`.

    Markdown.pl [ −−html4tags ] [ −−version ] [ −shortversion ] [ file ... ]

The `--html4tags` argument will just do nothing, but not be processed as a file.

    // In com/tristanhunt/knockoff/DefaultDiscounter.scala
    package com.tristanhunt.knockoff
    
    import java.io.{ File }
    import scala.util.logging.ConsoleLogger
    
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


### `Wholesaler` ###

I've started to include ideas from the [MultiMarkdown][] syntax system, but I didn't
want to mix that up with a normal Markdown format. The `Wholesaler` is going to be
the variation of the `Discounter`, plus **so much more**!

    // In com/tristanhunt/knockoff/extra/Wholesaler.scala
    package com.tristanhunt.knockoff.extra

    import com.tristanhunt.knockoff.{ Block, Paragraph, Discounter }
    import com.tristanhunt.knockoff.latex.{ LatexDiscounter, LatexWriter }
    
    trait Wholesaler extends Discounter with MetaDataConverter
      with MetaDataXHTMLWriter with LatexDiscounter with LatexWriter
      with SCAMLDiscounter with SCAMLLatexWriter


### `DefaultWholesaler` ###

Another console wrapping application. This one has to be called explicitly.

    // In com/tristanhunt/knockoff/extra/DefaultWholesaler.scala
    package com.tristanhunt.knockoff.extra

    import java.io.File
    import scala.util.logging.ConsoleLogger
    
    object DefaultWholesaler extends Wholesaler with ConsoleLogger {
      def main( args : Array[ String ] ) : Unit = try {
        if ( args.contains("--version") ) {
          Console.err.print( "DefaultWholesaler " )
        }
        if ( args.contains("--version") || args.contains("-shortversion") ) {
          Console.err.println( "0.7.0-10" )
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

[MultiMarkdown]: http://fletcherpenney.net/multimarkdown/users_guide/multimarkdown_syntax_guide/