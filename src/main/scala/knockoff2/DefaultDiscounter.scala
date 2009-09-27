package knockoff2

import scala.util.logging.ConsoleLogger

object DefaultDiscounter extends Discounter with ConsoleLogger {
  def main( args : Array[ String ] ) {
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
  }
  
  private def readText( fileName : String ) : String =
    io.Source.fromFile( fileName ).mkString("")
}
