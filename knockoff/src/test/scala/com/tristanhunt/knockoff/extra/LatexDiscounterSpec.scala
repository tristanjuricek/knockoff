package com.tristanhunt.knockoff.extra

import com.tristanhunt.knockoff._
import org.scalatest.fixture.FixtureFlatSpec
import org.scalatest.matchers.MustMatchers

class LatexDiscounterSpec extends FixtureFlatSpec with MustMatchers {
  
  behavior of "LatexDiscounter"
  
  type FixtureParam = LatexDiscounter
  
  class TestLatexDiscounter extends LatexDiscounter
  
  def withFixture( test : OneArgTest ) {
    test( new TestLatexDiscounter )
  }

  private def spanList( discounter : Discounter, src : String ) : List[Span] = {
    val blocks = discounter.knockoff( "A series $s_1, s_2$ is simple" )
    val para = blocks.first.asInstanceOf[Paragraph]
    return para.spans.toList
  }
  
  it should "read latex with underscores" in { discounter =>
    val actual = spanList( discounter, "A series $s_1, s_2$ is simple" )
    val expected = List( Text("A series "),
                         LatexSpan("$s_1, s_2$"),
                         Text(" is simple") )
    actual must equal ( expected )
  }
}
