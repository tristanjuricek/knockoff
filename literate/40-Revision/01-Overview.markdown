# Overview of The Knockoff Implementation #

The next major revision of Knockoff takes several ideas from [Pandoc][], a
Haskell-based system. This turns knockoff into a markup conversion system, from just
a [Markdown][] based system.

The object model will also go through more refinement, to focus on making usage
easier. The current system does not handle filtering tasks in a very simple way,
even with Scala code. On top of this object model will be methods to enable very
simple usage in non-Scala JVM languages.



## MarkupReaders and MarkupWriters ##

The basic system is broken into things that read markup into the object model, and
things that write out the object model to markup.

    val blocks = readMarkdown( markdownString )
    writeXHTML( blocks )

It's expected that your converter class will be a grouping of different readers
and writers for different conversion passes. For example, a converter can be built
by stringing them together.
  
    object MyConverter extends MarkdownMarkupReader with XHTMLMarkupWriter {
      /** Convert a markdown string to Scala XHTML */
      def markdownToXHTML( markdownString : String ) : Elem =
        writeXHTML( readMarkdown( markdownString ) )
    }

Note that converters do not necessarily need to read 1 kind of data type. While it
will be common to read from a `String` source, converting to an `xml.Elem` makes
just as much sense.

Alternative methods on traits will typically append the simpler data type to the
method name. So the above might want create a more Java-friendly class that only
uses `String`s.

    class MyConverter extends MarkdownMarkupReader with XHTMLMarkupWriter {
      /** Convert a markdown string to Scala XHTML */
      def markdownToXHTML( markdownString : String ) : String =
        writeXHTMLString( readMarkdown( markdownString ) )
    }

The read method should be some kind of sequence, currently, it's recommended to use
a `Stream`, in case the application only wants to "peer" into the front matter of
the document.

### Using the Object Model In Between

Use of the [object model][obj] is restricted to Scala logic, but may be useful for
doing simple queries and transformations of data. Lots of case classes and
extractors are defined to create friendly usage of the data contained in the object
model.

    // What's the text of the first header in the document?
    val headerText = blocks match {
      case Headers( first, _* ) => first.content.text
      case _ => "Default header"
    }



## Basic Console Utilities ##

    // In com/tristanhunt/knockoff2/ConsoleUtilities.scala
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




[pandoc]: http://johnmacfarlane.net/pandoc/ "Pandoc"
[markdown]: http://daringfireball.net/projects/markdown/ "Markdown Syntax Overview"
[obj]: 10-Object_Model.html