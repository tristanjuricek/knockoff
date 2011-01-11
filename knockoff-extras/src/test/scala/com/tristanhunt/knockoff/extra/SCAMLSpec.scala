package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff._
import org.scalatest._
import org.scalatest.matchers._

class SCAMLSpec extends Spec with ShouldMatchers {
  
  val discounter = DefaultWholesaler
  import discounter._
  
  describe( "SCAML Parser" ) {
    // See the SCAML specification
  }
}
