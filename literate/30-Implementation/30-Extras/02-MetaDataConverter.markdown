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

The values of the map shoudn't contain the separating newline.

There is no XML representation of `MetaData`. Conversion to header elements is
left to your program to interpret.

    // In com/tristanhunt/knockoff/extra/MetaData.scala
    package com.tristanhunt.knockoff.extra
    // See the MetaData imports
    
    class   MetaData( val data : Map[ String, String ], val position : Position )
    extends SimpleBlock {
      
      val span : SpanSeq = new Text( markdown )
      
      /** This should be an empty element. */
      val xml : Node = new Group( new NodeBuffer )
      
      val markdown : String =
        data.map{ case ((k,v)) => k + ": " + v }.mkString("\n")
    }
    // The MetaData imports
    
    import scala.util.parsing.input.Position
    import scala.xml.{ Group, Node, NodeBuffer }


## `MetaDataConverter` ##

    // In com/tristanhunt/knockoff/extra/MetaDataConverter.scala
    package com.tristanhunt.knockoff.extra
    // See the MetaDataConverter imports
    
    trait MetaDataConverter {
      
      /**
        @param  para The paragraph to be converted. We use the trimmed markdown 
                     value to determine the data.
        @return Some metadata equivalent of the paragraph. Or, None.
       */
      def toMetaData( para : Paragraph ) : Option[ MetaData ] =
        parseLine( para.markdown.trim.split("\n"), new ListBuffer )
          .map( new MetaData( _, para.position ) )
      
      /**
        Creates the string map that becomes the main data for MetaData.
      
        @param in  Each line of markdown content, minus the trailing newlines.
        @param out The current "chunks" of metadata. Each chunk can contain multiple
                   lines.
        @return The parsed Metadata when every line can be chunked, or None.
        */
      private def parseLine( in : Seq[ String ], out : ListBuffer[ String ] )
                            : Option[ Map[ String, String ] ] = {
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

      private def toMetaDataMap( out : Seq[ String ] ) : Map[ String, String ] =
        Map( out.map { chunk =>
              val idx = chunk.indexOf(':')
              ( chunk.substring(0, idx).trim, chunk.substring(idx + 1) )
             } : _* )
    }
    
    // The MetaDataConverter imports
    
    import scala.collection.mutable.ListBuffer


## `MetaDataConverterSpec` ##

    // In test com/tristanhunt/knockoff/extra/MetaDataConverterSpec.scala
    package com.tristanhunt.knockoff.extra
    // See the MetaDataConverterSpec imports
    
    class MetaDataConverterSpec extends Spec with ShouldMatchers with Wholesaler {

      describe("MetaDataConverter") {
        it("should convert a Paragraph to MetaData") {
          val content = "key 1 : value\n" +
                        "key2:value\n" +
                        "key 3 : long  \n" +
                        "        value\n" +
                        "key4:\n" +
                        ":oops"

          val metaData =
            toMetaData( new Paragraph( new Text( content ), NoPosition ) )
            .getOrElse( error("Did not convert the content to MetaData") )
          
          metaData.data should equal (
            Map( "key 1" -> " value",
                 "key2"  -> "value",
                 "key 3" -> " long  \n        value",
                 "key4"  -> "",
                 ""      -> "oops" )
          )
        }
      }
    }
    
    // The MetaDataConverterSpec imports
    
    import org.scalatest.Spec
    import org.scalatest.matchers.ShouldMatchers
    import scala.util.parsing.input.NoPosition