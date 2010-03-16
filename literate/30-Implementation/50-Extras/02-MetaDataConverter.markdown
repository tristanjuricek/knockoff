# `MetaData` and `MetaDataConverter` #

Methods to convert `Paragraph` to `MetaData`. A `Paragraph` is `MetaData` if all
lines can be converted to key/value strings.

A key value string is a line that has two text sections divided by a colon.

    key : value

Spaces around the key will be trimmed.

A key value can have a trailing line appended to the value, only if the previous
line ends with two spaces and then a newline.


## `MetaData` ##

`MetaData` is really just a `Map` of strings. But, we want to return it in the
`BlockSeq`, so it's a special kind of `Block`.

The values of the map shouldn't contain the separating newline.

There is no XML representation of `MetaData`. Conversion to header elements is
left to your program to interpret.

    // The MetaData block
    case class MetaData( data : Map[String, String], position : Position )
    extends Block
    
    // In com/tristanhunt/knockoff/extra/MetaData.scala
    package com.tristanhunt.knockoff.extra
    
    import com.tristanhunt.knockoff.{ Block }
    import scala.util.parsing.input.Position
    
    // See the MetaData block

## `MetaDataConverter` ##

    // The MetaDataConverter
    trait MetaDataConverter extends Logged {
      
      /**
        @param  para The paragraph to be converted. We use the trimmed markdown 
                     value to determine the data.
        @return Some metadata equivalent of the paragraph. Or, None.
       */
      def toMetaData( para : Paragraph ) : Option[MetaData] = {
        def toString( span : Span ) = span match {
          case Text( content ) => content
          case _ => { log( "Ignoring non-Text content in Paragraph" ); "" }
        }
        val content = ( "" /: para.spans.map( toString(_) ) )( _ + _ )
        parseLine( content.trim.split("\n"), new ListBuffer )
          .map( new MetaData( _, para.position ) )
      }
      
      /**
        Creates the string map that becomes the main data for MetaData.
      
        @param in  Each line of markdown content, minus the trailing newlines.
        @param out The current "chunks" of metadata. Each chunk can contain multiple
                   lines.
        @return The parsed Metadata when every line can be chunked, or None.
        */
      private def parseLine( in : Seq[String], out : ListBuffer[String] )
                           : Option[ Map[String, String] ] = {
        if ( in.isEmpty ) return Some( toMetaDataMap( out ) )
        in.first.indexOf(':') match {
          case -1 =>
            if ( out.isEmpty || ! out.last.endsWith("  ") ) return None
            else {
              val newChunk = out.last + "\n" + in.first
              out.trimEnd(1)
              out += newChunk
            }
          case idx => out += in.first
        }
        parseLine( in.drop(1), out )
      }

      private def toMetaDataMap( out : Seq[String] ) : Map[String, String] =
        Map( out.map {
                chunk =>
                val idx = chunk.indexOf(':')
                ( chunk.substring(0, idx).trim, chunk.substring(idx + 1) )
             } : _* )
    }
    
    // In com/tristanhunt/knockoff/extra/MetaDataConverter.scala
    package com.tristanhunt.knockoff.extra

    import com.tristanhunt.knockoff.{ Paragraph, Span, Text }
    import scala.collection.mutable.ListBuffer
    import scala.util.logging.{ Logged }
    
    // See the MetaDataConverter


## `MetaDataConverterSpec` ##

    // In test com/tristanhunt/knockoff/extra/MetaDataConverterSpec.scala
    package com.tristanhunt.knockoff.extra
    // See the MetaDataConverterSpec imports
    
    class MetaDataConverterSpec extends Spec with ShouldMatchers with Wholesaler {

      describe("MetaDataConverter") {
        it("should convert a Paragraph to MetaData") {
          val content = """key 1 : value
                          |key2:value
                          |key 3 : long  
                          |        value
                          |key4:
                          |:oops""".stripMargin

          val metaData =
            toMetaData( Paragraph( List( Text(content) ), NoPosition ) )
              .getOrElse( error("Did not convert the content to MetaData") )
          
          metaData.data should equal (
            Map( "key 1" -> " value",
                 "key2"  -> "value",
                 "key 3" -> " long  \n        value",
                 "key4"  -> "",
                 ""      -> "oops" ) )
        }
      }
    }
    
    // The MetaDataConverterSpec imports
    
    import com.tristanhunt.knockoff.{ Paragraph, Text }
    import org.scalatest.Spec
    import org.scalatest.matchers.ShouldMatchers
    import scala.util.parsing.input.NoPosition



## `MetaDataXHTMLWriter` ##

If you use the `MetaDataConverter` like the `Wholesaler` does, you'll need to 
use this extension to the `XHTMLWriter` or face match errors.

    // In com/tristanhunt/knockoff/extra/MetaDataXHTMLWriter.scala
    package com.tristanhunt.knockoff.extra
    
    import com.tristanhunt.knockoff._
    import scala.xml.{ Group, Node }
    
    trait MetaDataXHTMLWriter extends XHTMLWriter {
     
      override def blockToXHTML : Block => Node = block => block match {
        case MetaData( data, _ ) => Group( Nil )
        case _ => super.blockToXHTML( block )
      }
    }