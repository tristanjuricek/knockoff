package com.tristanhunt.knockoff.extra

import org.scalatest.fixture.FixtureFlatSpec
import org.scalatest.matchers.MustMatchers

class LatexXHTMLWriterSpec extends FixtureFlatSpec with MustMatchers {
  
  behavior of "LatexXHTMLWriter"
  
  type FixtureParam = LatexDiscounter with LatexXHTMLWriter
  
  class TestDiscounter extends LatexDiscounter with LatexXHTMLWriter
  
  def withFixture( test : OneArgTest ) {
    test( new TestDiscounter )
  }
  
  it should "write out MathML inside a paragraph" in { discounter =>
    val actual = htmlString( discounter, "A sequence $s_1, s_2 = N * M$ is *cool*." )
    actual must equal (
      "<p>A sequence " +
      "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" +
        "<msub><mi>s</mi><mn>1</mn></msub><mo>,</mo>" +
        "<msub><mi>s</mi><mn>2</mn></msub><mo>=</mo>" +
        "<mi>N</mi><mo>*</mo><mi>M</mi></math>" +
      " is <em>cool</em>.</p>"  )
  }
  
  private def htmlString( discounter : FixtureParam, src : String ) : String = {
    val blocks = discounter.knockoff( src )
    val xhtml = discounter.toXHTML( blocks )
    val writer = new java.io.StringWriter
    scala.xml.XML.write( writer, xhtml, "utf-8", false, null )
    return writer.toString
  }
}
