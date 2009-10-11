package knockoff

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
