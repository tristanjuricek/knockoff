package com.tristanhunt.knockoff

import org.scalatest.testng._
import org.testng.Assert._
import org.testng.annotations._

class InlineHTMLSplitterTest extends TestNGSuite {
 
    @Test
    def basicInlineHTMLCheck {
     
        val src = """This is a block with `a < b` code and <span class="fuck">Some basic <b>HTML</b></span>. And [a link](http://foo.bar)"""
        
        val actual = InlineHTMLSplitter.split(src)
        
        val expected = List(Text("This is a block with `a < b` code and "),
            HTML("<span class=\"fuck\">Some basic <b>HTML</b></span>"),
            Text(". And [a link](http://foo.bar)"))
            
        assertTrue(actual sameElements expected)
    }
}