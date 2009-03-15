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
 
    def setextHeader1 {
        
        val src = "heading 1\n=======\n \nBody text."
    
        val expected = List(Header("heading 1", 1), TextBlock("Body text."))
        
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        assertTrue(expected sameElements actual)
    }
    
    def setextHeader2 {
        
        val src = "heading 2\n--------\n \nBody text."
    
        val expected = List(Header("heading 2", 2), TextBlock("Body text."))
        
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some(list) => list
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
            
            val expected = List(Header("Heading # Value", level))
            
            val actual1 = KnockOff.parse(src1) match {
                case Some(list) => list
                case None  => {fail("ATX Header Parse Failed: " + src1); null}
            }
            
            val actual2 = KnockOff.parse(src2) match {
                case Some(list) => list
                case None  => {fail("ATX Header Parse Failed: " + src2); null}
            }
            
            assertTrue(expected sameElements actual1)
            assertTrue(expected sameElements actual2)
        })
    }
    
    def htmlBlock = {
     
        val src = """  a text block
  
<div>
     <span>To HTML</span>
</div>"""

        val actual:List[Block] = KnockOff.parse(src) match {
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        val expected = List(TextBlock("  a text block\n"), HTMLBlock("""<div>
     <span>To HTML</span>
</div>"""))

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
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }

        val expected = List(
            TextBlock("A list\n"),
            BulletListBlock(List("*  Item one\n", "*  Item two\n")),
            TextBlock("Another list\n"),
            BulletListBlock(List("- two 1\n", "- two 2\n", "- two 3\n")),
            TextBlock("A third list\n"),
            BulletListBlock(List("+  Hey this is a huge\nmultiline list that might\nget formatted\n",
                "+  Yeah this is\n   totally formatted\n",
                "+  Psht, whatever")))
        
        assertTrue(actual sameElements expected)
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
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        val expected = List(
            Header("A numbered list", 3),
            NumberedListBlock(List("1. First\n", "2. Second\nWith formatting\n", "1. Third with a\n   wrong number\n")),
            TextBlock("Second list\n"),
            NumberedListBlock(List("1.  You suck\n", "2.  Ass munch\n")))
        
        assertTrue(actual sameElements expected)
    }
    
    def blockquoted = {
     
        val src = """Block quote

> This is a nice big blockquote.
>
> There are multiple lines.

> Here is another block
> quote.
> with one line"""

        val actual:List[Block] = KnockOff.parse(src) match {
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }
        
        val expected = List(
            TextBlock("Block quote\n"),
            Blockquote("""> This is a nice big blockquote.
>
> There are multiple lines.
"""),
            Blockquote("""> Here is another block
> quote.
> with one line"""))

        assertTrue(actual sameElements expected)
    }
    
    
    def codeBlock = {
     
        val src = """# Code Block #

    <html>
        <head></head>
        <body></body>
    </html>

Perhaps some commands:

     $ Command one
    
     ./ a line!
"""
        val actual:List[Block] = KnockOff.parse(src) match {
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }

        val expected = List(
            Header("Code Block", 1),
            CodeBlock("""    <html>
        <head></head>
        <body></body>
    </html>
"""),
            TextBlock("Perhaps some commands:\n"),
            CodeBlock("""     $ Command one
    
     ./ a line!
"""))

        assertTrue(actual sameElements expected)
    }
    
    def horizontalRule = {
     
        val src = """A Thingy
--------

- - -

Something else

__________"""

        val actual:List[Block] = KnockOff.parse(src) match {
            case Some(list) => list
            case None => {
                fail("src not parsed: " + src)
                null
            }
        }

        val expected = List(
            Header("A Thingy", 2),
            HorizontalRule("- - -\n"),
            TextBlock("Something else\n"),
            HorizontalRule("__________"))

        assertTrue(actual sameElements expected)
    }

}