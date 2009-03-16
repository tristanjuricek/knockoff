package com.tristanhunt.knockoff

import org.testng.Assert._
import org.testng.annotations._

@Test
class InlineHTMLSplitterTest {
 
    def basicInlineHTMLCheck {
     
        val src = "This is a block with `a < b` code and <span>Some basic <b>HTML</b></span>. And [a link](http://foo.bar)"
        
        val actual = InlineHTMLSplitter.split(src)
        
        val expected = List(Text("This is a block with `a < b` code and "),
            HTML("<span>Some basic <b>HTML</b></span>"),
            Text(". And [a link](http://foo.bar)"))
            
        assertTrue(actual sameElements expected)
    }
}