package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff.{ Paragraph, Text }
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.util.parsing.input.NoPosition

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

