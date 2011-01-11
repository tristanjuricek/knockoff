package com.tristanhunt.knockoff.extra

import org.scalatest._
import org.scalatest.matchers._
import org.w3c.tidy.{ Tidy }
import java.io._
import scala.collection.mutable.{ ListBuffer }
import scala.xml.{ Node, XML }

class ExtrasIntegrationTests extends Spec with ShouldMatchers {
    
  val basedir = "knockoff-extras/src/test/resources/tests"
  
  val jtidy = {
    val tidy = new Tidy
    tidy.setXHTML( true )
    tidy.setShowWarnings( false )
    tidy.setShowErrors( 0 )
    tidy.setQuiet( true )
    tidy.setForceOutput( true )
    tidy
  }
  
  def file( src : String, child : String ) = new File( src, child )
  
  def writeString( node : Node ) : String = {
    val sw = new StringWriter
    XML.write( sw, node, "UTF-8", false, null )
    sw.toString
  }
  
  def tidy( src : String ) : String = {
    val reader = new StringReader( src )
    val writer = new StringWriter
    jtidy.parse( reader, writer )
    writer.toString
  }
  
  def normalizeSpace( str : String ) : String =
    str.replaceAll( "\\s+", " " )
  
  class FileHelper( wrapped : File ) {
    
    /** Try to match files based on the fromExt to the toExt. */
    def listTests( fromExt : String, toExt : String ) : Seq[ (File, File) ] = {
      val endsWithFrom = new FileFilter {
        def accept( f : File ) = f.getName endsWith fromExt
      }
      val files = wrapped.listFiles( endsWithFrom )
      val mapName : File => File =
        from => file( from.getParent,
                      from.getName.replace( fromExt, toExt ) )
      files.map( f => (f, mapName(f)) )
    }
    
    /** A version that will actually compile under 2.8. */
    def text : String = {
      val reader = new BufferedReader( new FileReader( wrapped ) )
      var lines = new ListBuffer[String]
      try {
        var line = reader.readLine
        while( line != null ) {
          lines += line
          line = reader.readLine
        }
      } finally {
        reader.close
      }
      lines.mkString("\n")
    }
  }
  
  implicit def FileHelper( f : File ) = new FileHelper( f )
  
  describe( "Wholesaler" ) {
    import DefaultWholesaler._
    it( "should convert tests from Markdown to XHTML" ) {
      val dir = file( basedir, "wholesaler_markdown-xhtml" )
      dir.listTests(".text", ".html").foreach { case (from, to) =>
        from should be ('exists)
        to should be ('exists)
        println( "Test: " + from.getName )
        val fromXHTML = writeString( toXHTML( knockoff( from.text ) ) )
        tidy( fromXHTML ) should equal ( tidy( to.text ) )
      }          
    }
    
    it( "should convert tests from Markdown to LaTeX" ) {
      val dir = file( basedir, "wholesaler_markdown-latex" )
      dir.listTests(".markdown", ".tex").foreach { case (from, to) =>
        from should be ('exists)
        to should be ('exists)
        println( "Test: " + from.getName )
        val fromText = toLatex( knockoff( from.text ) )
        fromText should equal ( to.text )
      }
    }
    
    it( "should convert tests from Markdown to SCAML" ) {
      val dir = file( basedir, "wholesaler_markdown-scaml" )
      dir.listTests(".txt", ".scaml").foreach { case (from, to) =>
        from should be ('exists)
        to should be ('exists)
        println( "Test: " + from.getName )
        val fromSCAML = toSCAML( knockoff( from.text ) ).trim
        fromSCAML should equal( to.text )
      }
    }
  }
}


