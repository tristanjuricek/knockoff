package com.tristanhunt.knockoff2

import scala.util.logging.{ ConsoleLogger }
import scala.util.parsing.input.{ Reader }    
import scala.xml._

object MarkdownConsole extends MarkdownReader with XHTMLWriter with ConsoleLogger {
 
  def main( args : Array[String] ) {
    args.foreach( arg => print( writeXHTML( readMarkdown( read( arg ) ) ) ) )
  }
  
  def print( nodes : NodeBuffer ) {
    nodes.foreach( n => println(n.toString) )
  }
  
  override def logParsingError[T]( msg : String, reader : Reader[T] ) : Unit =
    println( Console.RED + "Warning at " + reader.pos.longString + ":\n" + msg
             + Console.RESET )
  
  def read( file : String ) : String = {
    io.Source.fromFile( new java.io.File( file ) ).getLines().mkString("\n")
  }
}
