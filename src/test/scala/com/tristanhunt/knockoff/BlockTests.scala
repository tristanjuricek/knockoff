package com.tristanhunt.knockoff

import org.testng.Assert._
import org.testng.annotations._

/**
 * Mostly, some tests I've used as building up the library. Here to keep me sane.
 *
 * @author Tristan Juricek <juricek@emarsys.com>
 */
@Test
class BlockTests {
    
    import collection.immutable._
 
    def setextHeader1 {
        
        val src = "heading 1\n=======\n \nBody text."
    
        val expected = List(
            Header(List(Text("heading 1")), 1),
            Paragraph(List(Text("Body text."))))
        
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        assertTrue(expected sameElements actual)
    }
    
    def setextHeader2 {
        
        val src = "heading 2\n--------\n \nBody text."
    
        val expected = List(
            Header(List(Text("heading 2")), 2),
            Paragraph(List(Text("Body text."))))
        
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        assertTrue(expected sameElements actual)
    }


    def atxHeader = {
     
        Iterator.range(1,7).foreach(level => {
          
            val sb = new StringBuilder
            
            Iterator.range(0, level).foreach(i => sb.append('#'))
            
            val src1 = sb.toString + " Heading # Value "
            val src2 = sb.toString + " Heading # Value " + sb.toString
            
            val expected = List(
                Header(List(Text("Heading # Value")), level))
            
            val actual1 = KnockOff.parse(src1) match {
                case Some((list, _)) => list
                case None  => {fail("ATX Header Parse Failed: " + src1); null}
            }
            
            val actual2 = KnockOff.parse(src2) match {
                case Some((list, _)) => list
                case None  => {fail("ATX Header Parse Failed: " + src2); null}
            }
            
            assertTrue(expected sameElements actual1)
            assertTrue(expected sameElements actual2)
        })
    }
    
    def htmlMkBlock = {
     
        val src = """  a text block
  
<div>
     <span>To HTML</span>
</div>"""

        val actual:List[Block] = KnockOff.parse(src).get._1
        
        val expected = List(
            Paragraph(List(Text("  a text block\n"))),
            HTMLBlock(HTML("<div>\n     <span>To HTML</span>\n</div>")))

        assertTrue(expected sameElements actual)
    }
    
    def bulletLists = {
     
        val src = """A list
        
*  Item one
*  Item two

Another list

- two 1

- two 2

- two 3

A third list

+  Hey this is a huge
multiline list that might
get formatted
+  Yeah this is
   totally formatted
+  Psht, whatever"""

        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }

        val expected = List(
            Paragraph(List(Text("A list\n"))),
            UnorderedBlockList(List(
                Paragraph(List(Text("Item one\n"))),
                Paragraph(List(Text("Item two\n"))))),
            Paragraph(List(Text("Another list\n"))),
            UnorderedBlockList(List(
                Paragraph(List(Text("two 1\n"))),
                Paragraph(List(Text("two 2\n"))),
                Paragraph(List(Text("two 3\n"))))),
            Paragraph(List(Text("A third list\n"))),
            UnorderedBlockList(List(
                Paragraph(List(Text("Hey this is a huge\nmultiline list that might\nget formatted\n"))),
                Paragraph(List(Text("Yeah this is\n   totally formatted\n"))),
                Paragraph(List(Text("Psht, whatever"))))))
        
        assertTrue(actual sameElements expected,
            "[actual sameElements expected == false] actual:" + actual + ", expected:" + expected)
    }

    def numberedLists = {
     
        val src = """### A numbered list ###
        
1. First
2. Second
With formatting
1. Third with a
   wrong number
   
Second list

1.  You suck

2.  Ass munch
"""
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        val expected = List(
            Header(List(Text("A numbered list")), 3),
            OrderedBlockList(List(
                Paragraph(List(Text("First\n"))),
                Paragraph(List(Text("Second\nWith formatting\n"))),
                Paragraph(List(Text("Third with a\n   wrong number\n"))))),
            Paragraph(List(Text("Second list\n"))),
            OrderedBlockList(List(
                Paragraph(List(Text("You suck\n"))),
                Paragraph(List(Text("Ass munch\n"))))))
        
        assertTrue(actual sameElements expected,
            "[actual sameElements expected == false] actual:" + actual + ", expected:" + expected)
    }
    
    def blockquoted = {
     
        val src = """MkBlock quote

> This is a nice big blockquote.
>
> There are multiple lines.

> Here is another block
> quote.
> with one line"""

        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        val expected = List(
            Paragraph(List(Text("MkBlock quote\n"))),
            Blockquote(List(
                Paragraph(List(Text("This is a nice big blockquote.\n"))),
                Paragraph(List(Text("There are multiple lines.\n"))))),
            Blockquote(List(
                Paragraph(List(Text("Here is another block\nquote.\nwith one line"))))))

        assertTrue(actual sameElements expected,
            "[actual sameElements expected == false] actual:" + actual + ", expected:" + expected)
    }
    
    
    def codeBlock = {
     
        val src = """# Code MkBlock #

    <html>
        <head></head>
        <body></body>
    </html>

Perhaps some commands:

     $ Command one
    
     ./ a line!
"""
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }

        val expected = List(
            Header(List(Text("Code MkBlock")), 1),
            CodeBlock(Text("""<html>
    <head></head>
    <body></body>
</html>
""")),
            Paragraph(List(Text("Perhaps some commands:\n"))),
            CodeBlock(Text(""" $ Command one

 ./ a line!
""")))

        assertTrue(actual sameElements expected)
    }
    
    def horizontalRule = {
     
        val src = """A Thingy
--------

- - -

Something else

__________"""

        val actual:List[Block] = KnockOff.parse(src) match {
            case Some((list, _)) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }

        val expected = List(
            Header(List(Text("A Thingy")), 2),
            HorizontalRule(),
            Paragraph(List(Text("Something else\n"))),
            HorizontalRule())

        assertTrue(actual sameElements expected)
    }

    def linkDefinition = {
     
        val src = """
This is a link definition

[An ID] http://example.com/foo?bar=bat

[An ID 2] http://example.com/foo?bar=bat "Basic "Title" Link"
[An ID 3]    http://example.com/foo?bar=bat 
    "Basic "Title" Link2"

Some variations:

 [1] http://example.com

  [2] http://example.com 'title'
   [3] http://google.com (why not?)

    [4] http://code.example.com (Yeah, this is not a link)
"""

//

        val (actualContent:List[Block], actualDefs:SortedMap[String, LinkDefinition]) =
            KnockOff.parse(src).get
        
        val expectedContent = List(
            Paragraph(List(Text("This is a link definition\n"))),
            Paragraph(List(Text("Some variations:\n"))),
            CodeBlock(Text("[4] http://code.example.com (Yeah, this is not a link)\n")))

        assertTrue(actualContent sameElements expectedContent)

        val expectedDefs = TreeMap(
            "An ID"     -> LinkDefinition("An ID", "http://example.com/foo?bar=bat", ""),
            "An ID 2"   -> LinkDefinition("An ID 2", "http://example.com/foo?bar=bat", """Basic "Title" Link"""),
            "An ID 3"   -> LinkDefinition("An ID 3", "http://example.com/foo?bar=bat", """Basic "Title" Link2"""),
            "1"         -> LinkDefinition("1", "http://example.com", ""),
            "2"         -> LinkDefinition("2", "http://example.com", "title"),
            "3"         -> LinkDefinition("3", "http://google.com", "why not?"))(_caseInsensitiveCheck)
        
        assertTrue(actualDefs sameElements expectedDefs,
            "[actualDefs sameElements expectedDefs == false]\n\tactualDefs:" + actualDefs + "\n\texpectedDefs:" + expectedDefs)
    }
    
    private def _caseInsensitiveCheck(key:String) = new Ordered[String] {
        def compare(toKey:String):Int = key compareToIgnoreCase toKey
    }
}