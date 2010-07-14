package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ Block, Paragraph, Discounter }

trait Wholesaler extends Discounter with MetaDataConverter
  with MetaDataXHTMLWriter with LatexDiscounter with LatexWriter with LatexXHTMLWriter
  with SCAMLDiscounter with SCAMLLatexWriter with SCAMLMetaDataWriter

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
