package com.tristanhunt.knockoff

import Imports._
import org.scalatest.testng._
import org.testng.Assert._
import org.testng.Assert.{ fail => failTest }
import org.testng.annotations._
import scala.collection.immutable._

/**
 * Mostly, some tests I've used as building up the library. Here to keep me sane.
 *
 * @author Tristan Juricek <juricek@emarsys.com>
 */
class BlockTests extends TestNGSuite {

    @Test
    def setextHeader1 {
        
        val src = "heading 1\n=======\n \nBody text."
    
        val expected = List(
            Header(List(Text("heading 1")), 1),
            Paragraph(List(Text("Body text."))))
        
        val actual:List[Block] = knockoff( src ) match {
            case KnockOff.Parsed(list) => list
            case f : KnockOff.Failed  => { fail(f.message); null }
        }
        
        assertTrue(expected sameElements actual)
    }
    
    @Test
    def setextHeader2 {
        
        val src = "heading 2\n--------\n \nBody text."
    
        val expected = List(
            Header( List( Text( "heading 2" ) ), 2 ),
            Paragraph( List( Text( "Body text." ) ) )
        )
        
        val actual:List[Block] = knockoff(src) match {
            case KnockOff.Parsed(list) => list
            case f : KnockOff.Failed => { fail(f.message); null }
        }
        
        assertTrue(expected sameElements actual)
    }
    
    /**
     * Use an immediately trailing paragraph text.
     */
    @Test
    def setextHeader3 {
       
        val src = "heading 1\n===========\nBody Text"
       
        val expected = List(
            Header( List( Text( "heading 1" ) ), 1 ),
            Paragraph( List( Text( "Body Text" ) ) )
        )
        
        val actual:List[ Block ] = knockoff( src ).get
        
        assertTrue(
            expected sameElements actual,
            "\nexpected : " + expected +
            "\nactual   : " + actual + "\n"
        )
    }


    @Test
    def atxHeader = {
     
        Iterator.range(1,7).foreach(level => {
          
            val sb = new StringBuilder
            
            Iterator.range(0, level).foreach(i => sb.append('#'))
            
            val src1 = sb.toString + " Heading # Value "
            val src2 = sb.toString + " Heading # Value " + sb.toString
            
            val expected = List(
                Header(List(Text("Heading # Value")), level))
            
            val actual1 = knockoff(src1) match {
                case KnockOff.Parsed( list ) => list
                case f : KnockOff.Failed  => { fail("ATX Header Parse Failed: " + f.message ); null}
            }
            
            val actual2 = knockoff(src2) match {
                case KnockOff.Parsed(list) => list
                case f : KnockOff.Failed  => {fail("ATX Header Parse Failed: " + f.message ); null}
            }
            
            assertTrue(expected sameElements actual1)
            assertTrue(expected sameElements actual2)
        })
    }
    
    @Test
    def htmlMkBlock = {
     
        val src = """  a text block
  
<div>
     <span>To HTML</span>
</div>"""

        val actual:List[Block] = knockoff(src).get
        
        val expected = List(
            Paragraph(List(Text("  a text block\n"))),
            HTMLBlock(HTML("<div>\n     <span>To HTML</span>\n</div>")))

        assertTrue(expected sameElements actual)
    }
    
    @Test
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

        val actual:List[Block] = knockoff(src) match {
            case KnockOff.Parsed( list ) => list
            case f : KnockOff.Failed => { fail(f.message); null }
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

    @Test
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
        val actual:List[Block] = knockoff(src) match {
            case KnockOff.Parsed(list) => list
            case f : KnockOff.Failed => { fail(f.message); null }
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
    
    @Test
    def blockquoted = {
     
        val src = """MkBlock quote

> This is a nice big blockquote.
>
> There are multiple lines.

> Here is another block
> quote.
> with one line"""

        val actual:List[Block] = knockoff(src) match {
            case KnockOff.Parsed(list) => list
            case f : KnockOff.Failed => { fail(f.message); null }
        }
        
        val expected = List(
            Paragraph(List(Text("MkBlock quote\n"))),
            Blockquote(List(
                Paragraph(List(Text("This is a nice big blockquote.\n"))),
                Paragraph(List(Text("There are multiple lines.\n"))))),
            Blockquote(List(
                Paragraph(List(Text("Here is another block\nquote.\nwith one line"))))))

        assertTrue(actual sameElements expected,
            "[actual sameElements expected == false]\n\tactual  :" + actual + "\n\texpected:" + expected)
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
        val actual:List[Block] = knockoff(src) match {
            case KnockOff.Parsed(list) => list
            case f : KnockOff.Failed => { fail(f.message); null }
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

    
    @Test
    def horizontalRule = {
     
        val src = """A Thingy
--------

- - -

Something else

__________
"""

        val actual:List[Block] = knockoff(src) match {
            case KnockOff.Parsed(list) => list
            case f : KnockOff.Failed => { fail(f.message); null }
        }

        val expected = List(
            Header(List(Text("A Thingy")), 2),
            HorizontalRule(),
            Paragraph(List(Text("Something else\n"))),
            HorizontalRule())

        assertTrue(actual sameElements expected)
    }


    @Test
    def linkDefinition = {
     
        val src = """
This is a [used link definition][an id]

[An ID]: http://example.com/foo?bar=bat

[Variation 1][an id 2] [Variation 2][an id 3].

[An ID 2]: http://example.com/foo?bar=bat "Basic "Title" Link"
[An ID 3]:    http://example.com/foo?bar=bat 
    "Basic "Title" Link2"

[one][1] [two][2] [three][3]

 [1]: http://example.com

  [2]: http://example.com 'title'
   [3]: http://google.com (why not?)

    [4]: http://code.example.com (Yeah, this is not a link)
"""

//

        val actual:List[Block] = knockoff(src).get
        
        val expected = List(
            Paragraph(List(
                Text("This is a "),
                Link(List(Text("used link definition")), "http://example.com/foo?bar=bat", ""),
                Text("\n"))),
            Paragraph(List(
                Link(List(Text("Variation 1")), "http://example.com/foo?bar=bat", """Basic "Title" Link"""),
                Text(" "),
                Link(List(Text("Variation 2")), "http://example.com/foo?bar=bat", """Basic "Title" Link2"""),
                Text(".\n"))),
            Paragraph(List(
                Link(List(Text("one")), "http://example.com", ""),
                Text(" "),
                Link(List(Text("two")), "http://example.com", "title"),
                Text(" "),
                Link(List(Text("three")), "http://google.com", "why not?"),
                Text("\n"))),
            CodeBlock(Text("[4]: http://code.example.com (Yeah, this is not a link)\n")))

        assertTrue(actual sameElements expected,
            "[actual sameElements expected == false]\n\tactual  :" + actual + "\n\texpected:" + expected)
    }


    /**
     * Trying to identify a problem where whitespace leading into a separate
     * element is causing the block parser to blow chunks.
     */
    @Test
    def codeBlockSpacing {

        val codeBlockToHeader =
            "    code block\n" +
            "    \n" +    // empty line sometimes left by editors with space
            "## Header\n"
    
        val actual = knockoff( codeBlockToHeader ) match {
            case KnockOff.Parsed( blocks ) => blocks
            case KnockOff.Failed( message ) => { fail( message ); null }
        }
        
        val expected = List(
            CodeBlock( Text( "code block\n" ) ),
            Header( List( Text( "Header" ) ), 2 )
        )
        
        assertTrue(
            actual sameElements expected,
            "\nactual   : " + actual +
            "\nexpected : " + expected
        )
    }
    
    /**
        Two code blocks separated by an empty line should really be treated like one code block.
    */
    @Test
    def multiCodeBlocks {
        
        val multiLines =
            "    code line 1\n" +
            "\n" +
            "    code line 2\n"
    
        val actual = knockoff( multiLines ).get
        
        val expected = List(
            CodeBlock( Text( "code line 1\n\ncode line 2\n" ) )
        )
        
        assertTrue(
            actual sameElements expected,
            "\nactual   : " + actual +
            "\nexpected : " + expected
        )
    }
    
    
    /**
     * A coworker just loved to have lots of nested blocks right before a normal line, which looks
     * like crap, and, explodes the parser. If people want things to look like crap, fine, but I
     * really want to be as forgiving as possible.
     */
    @Test
    def uglyAssSpacing {
        
        val spaceBeforeNothing =
            "1. A list item\n" +
            "\n" +
            "        A code in a list\n" +
            "2. Another list item"
        
        val expected = List( OrderedComplexBlockList( List(
            List(
                Paragraph( List( Text( "A list item\n" ) ) ),
                CodeBlock( Text( "A code in a list\n" ) )
            ),
            List(
                Paragraph( List( Text( "Another list item" ) ) )
            )
        ) ) )
        
        knockoff( spaceBeforeNothing ) match {
            case KnockOff.Parsed( actual ) => {
                assertTrue( actual sameElements expected,
                    "\nactual   : " + actual +
                    "\nexpected : " + expected
                )
            }
            case KnockOff.Failed( msg ) => failTest( msg )
        }
    }
    
    /**
     * Trailing whitespace lines in code blocks should work fine.
     */
    @Test
    def extraCodeSpace {
        val example = {
            "Normal line.\n" +
            "\n" +
            "    code block\n" +
            "    \n" +
            "\n" +
            "Normal line."
        }
        
        val expected = List(
            Paragraph( List( Text( "Normal line.\n" ) ) ),
            CodeBlock( Text( "code block\n\n" ) ),
            Paragraph( List( Text( "Normal line." ) ) )
        )
        
        knockoff( example ) match {
            case KnockOff.Parsed( actual ) => {
                assertTrue( actual sameElements expected,
                    "\nactual   : " + actual +
                    "\nexpected : " + expected
                )
            }
            case KnockOff.Failed( message ) => failTest( message )
        }
    }
}

class TempTests extends TestNGSuite {
    @Test
    def linksReferenceStyleTest = _compare(
        MarkdownExamples.linksReferenceStyle,
        ConversionExamples.linksReferenceStyle
    )
    
    /**
     * Doing a couple of tricks so that each of the tests are just a bit easier to read.
     */
    protected def _compare(markdown:String, node:xml.Node) {
     
        assertEquals(
            { <div>{ knockoff( markdown ).get.toXML }</div> }.toString,
            { <div>{ node }</div> }.toString
        )
    }
    
    protected def _compare(markdown:String, expected:String) {
        
        assertEquals(
            { <div>{ knockoff( markdown ).get.toXML }</div> }.toString,
            "<div>" + expected + "</div>"
        )
    }
}