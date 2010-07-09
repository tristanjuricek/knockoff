# Part 3. Overview #

This part of Knockoff utilizes extensions that are probably very project-specific.
The parts in this section are only available under the `extra` namespace, which is
available if you use the `Wholesaler` trait instead of the `Discounter`.


## The Wholesaler ##

This is the "everything and the kitchen sink" trait for all features. If you don't
need something like SCAML, you probably want to create your own trait similarly.

    // The Wholesaler
    trait Wholesaler extends Discounter with MetaDataConverter
      with MetaDataXHTMLWriter with LatexDiscounter with LatexWriter with LatexXHTMLWriter
      with SCAMLDiscounter with SCAMLLatexWriter with SCAMLMetaDataWriter

### Default Wholesaler

Another console wrapping object alternative to the `DefaultDiscounter`.

    // The DefaultWholesaler
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

### Wholesaler.scala

    // In com/tristanhunt/knockoff/extra/Wholesaler.scala
    package com.tristanhunt.knockoff.extra

    import com.tristanhunt.knockoff.{ Block, Paragraph, Discounter }
    
    // See the Wholesaler
    
    import java.io.File
    import scala.util.logging.ConsoleLogger
    
    // See the DefaultWholesaler

[MultiMarkdown]: http://fletcherpenney.net/multimarkdown/users_guide/multimarkdown_syntax_guide/