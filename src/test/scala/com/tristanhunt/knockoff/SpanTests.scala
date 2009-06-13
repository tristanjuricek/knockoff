package com.tristanhunt.knockoff

import org.scalatest.testng._
import org.testng.Assert._
import org.testng.annotations._
import collection.immutable.TreeMap.empty

class SpanTests extends TestNGSuite {
 
    @Test
    def inlineHTML = {
     
        val actual = SpanParser(empty).parse("""A block with <span class="strong">Some <br><br> HTML</span> in it.""")
        
        val expected = List(
            Text("A block with "),
            HTML("""<span class="strong">Some <br><br> HTML</span>"""),
            Text(" in it."))
        
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def emphasisTest = {
        assertTrue(Emphasis(Nil) == Emphasis(Nil))
        assertTrue(Emphasis(List(Text("a"))) == Emphasis(List(Text("a"))))
        assertTrue(Emphasis(List(Text("b"))) != Emphasis(List(Text("a"))))
    }
    
    
    @Test
    def emphasisUnderscore = {
     
        val actual = SpanParser(empty).parse("""_emphasis_ possibly with _two words_""")
        
        val expected = List(
            Emphasis(List(Text("emphasis"))),
            Text(" possibly with "),
            Emphasis(List(Text("two words"))))
        
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def emphasisAsterix = {
     
        val actual = SpanParser(empty).parse("""*shocking _but_ true*""")
        
        val expected = List(
            Emphasis(List(
                Text("shocking "),
                Emphasis(List(Text("but"))),
                Text(" true"))))
        
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def strongUnderscore = {
        
        val actual = SpanParser(empty).parse("""This __is a *fucking* test__""")
        
        val expected = List(
            Text("This "),
            Strong(List(
                Text("is a "),
                Emphasis(List(Text("fucking"))),
                Text(" test"))))
                
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def strongAsterix { // != obelix
        
        val actual = SpanParser(empty).parse("""**hi** you **guy__**what__?""")
        
        val expected = List(
            Strong(List(Text("hi"))),
            Text(" you "),
            Strong(List(Text("guy__"))),
            Text("what__?"))
            
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def inlineLink {
     
        val actual = SpanParser(empty).parse("[First](http://example.com) link has no title." +
            "[The *second* link](http://example.com/foo?bar=bat \"Crappy \"Think\" Link\") has a title. And then, " +
            "of course, there's the [Empty]() empty link.")
        
        val expected = List(
            Link(List(Text("First")), "http://example.com", ""),
            Text(" link has no title."),
            Link(
                List(Text("The "), Emphasis(List(Text("second"))), Text(" link")),
                "http://example.com/foo?bar=bat",
                "Crappy \"Think\" Link"),
            Text(" has a title. And then, of course, there's the "),
            Link(List(Text("Empty")), "", ""),
            Text(" empty link."))
        
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def imageLink {
        
        val actual = SpanParser(empty).parse("![Alternative Text](http://path/to/image.jpg \"Title\")")
        
        val expected = List(ImageLink(List(Text("Alternative Text")), "http://path/to/image.jpg", "Title"))
        
        assertTrue(actual sameElements expected)
    }
    
    @Test
    def referenceLink {
        
        import collection.immutable.TreeMap
     
        val actual = SpanParser(
            TreeMap("1" -> LinkDefinition("1", "http://example.com", "Link Reference"))(other.FancyStrings.caseInsensitiveOrder))
            .parse("""[Link Ref][1] [No Link][2]""")
        
        val expected = List(
            Link(List(Text("Link Ref")), "http://example.com", "Link Reference"),
            Text(" [No Link][2]"))
        
        assertTrue(actual sameElements expected,
            "[actual sameElements expected == false]\n\tactual:" + actual + "\n\texpected:" + expected)
    }
}