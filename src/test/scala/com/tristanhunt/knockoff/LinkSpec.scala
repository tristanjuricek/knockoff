package com.tristanhunt.knockoff

import org.scalatest._
import org.scalatest.matchers._

class LinkSpec extends Spec with ShouldMatchers {
  
  val factory = new ElementFactory
  import factory._
 
  describe("Link") {
    it("should entitize html mailto: links") {
      val jdoe = link( text("jdoe"), "mailto:jdoe@example.com" )
      ( jdoe.xml \ "@href" ) should not equal {
        "mailto:jdoe@example.com"
      }
    }
  }
}
