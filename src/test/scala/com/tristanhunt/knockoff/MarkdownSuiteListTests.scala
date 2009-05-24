package com.tristanhunt.knockoff

import org.testng.annotations._
import org.testng.Assert._
import scala.xml._

/**
 * OK, so I split out the "ordered and unordered lists" stuff from the normal markdown test suite
 * because I needed to figure out how to do this nested parsing thing.
 */
@Test
class MarkdownSuiteListTests {

    
    def bulletsTight {
     
        val bullets = List( "*", "-", "+" )
     
        bullets.foreach ( bullet => {
     
            val source =
                bullet + " asterisk 1\n" +
                bullet + " asterisk 2\n" +
                bullet + " asterisk 3\n"

            val actual = <div>{ KnockOff.convert( source ).get }</div>.toString
        
            val expected = {
                "<div>" +
                    "<ul>" +
                        "<li>asterisk 1\n</li>" +
                        "<li>asterisk 2\n</li>" +
                        "<li>asterisk 3\n</li>" +
                    "</ul>" +
                "</div>"
            }
        
            assertEquals( actual, expected, "bullet = " + bullet )
            
        } )
    }
    

    /**
    
    Note: I have no clue why the official suite would *ever* want the loose wrapping to have the
    list items to have the embedded <p> elements. I'm not really supporting that by design. This
    output is identical to the "asterisksTight" mechanism.
    
    */    
    def bulletsLoose {
     
        val bullets = List( "*", "-", "+" )
     
        bullets.foreach( bullet => {
            
            val source = {
                bullet + " asterisk 1\n" +
                "\n" +
                bullet + " asterisk 2\n" +
                "   \n" +
                bullet + " asterisk 3\n"
            }

            val actual = <div>{ KnockOff.convert( source ).get }</div>.toString
        
            val expected = {
                "<div>" +
                    "<ul>" +
                        "<li>asterisk 1\n</li>" +
                        "<li>asterisk 2\n</li>" +
                        "<li>asterisk 3\n</li>" +
                    "</ul>" +
                "</div>"
            }
        
            assertEquals( actual, expected )
        } )
    }
    
    
    def orderedTight {
        
        val singleSpaced =
            "1. One\n" +
            "2. Two\n" +
            "3. Three\n"
        
        val doubleSpaced =
            "1.  One\n" +
            "2.  Two\n" +
            "3.  Three\n"
        
        val expected = {
            "<div>" +
                "<ol>" +
                    "<li>One\n</li>" +
                    "<li>Two\n</li>" +
                    "<li>Three\n</li>" +
                "</ol>" +
            "</div>"
        }
        
        val actualSingleSpaced = <div>{ KnockOff.convert( singleSpaced ).get }</div>.toString
        val actualDoubleSpaced = <div>{ KnockOff.convert( doubleSpaced ).get }</div>.toString
        
        assertEquals( actualSingleSpaced, expected )
        
        assertEquals( actualDoubleSpaced, expected )
    }
    
    
    def orderedLoose {
        
        val looseWithSpaces =
            "1. One\n" +
            "\n" +
            "2. Two\n" +
            "  \n" +
            "3. Three\n"
        
        val looseWithTabs =
            "1.\tOne\n" +
            "\n" +
            "2.\tTwo\n" +
            "  \n" +
            "3.\tThree\n"
        
        val expected = {
            "<div>" +
                "<ol>" +
                    "<li>One\n</li>" +
                    "<li>Two\n</li>" +
                    "<li>Three\n</li>" +
                "</ol>" +
            "</div>"
        }
        
        val actualWithSpaces    = <div>{ KnockOff.convert( looseWithSpaces ).get }</div>.toString
        val acutalWithTabs      = <div>{ KnockOff.convert( looseWithTabs ).get }</div>.toString
        
        assertEquals( actualWithSpaces, expected )
        
        assertEquals( acutalWithTabs, expected )
    }
    
    
    def multipleParagraphs {
        
        val source = {
            "1.\tItem 1, graf one.\n"+
            "\n" +
        	"\tItem 2. graf two. The quick brown fox jumped over the lazy dog's\n" +
        	"\tback.\n" +
            "\n" +
            "2.\tItem 2.\n" +
            "\n" +
            "3.\tItem 3.\n"
        }
        
        val expected = {
            "<div>" +
                "<ol>" +
                    "<li><p>Item 1, graf one.\n</p>" +
                    "<p>Item 2. graf two. The quick brown fox jumped over the lazy dog's\n" +
                    "back.\n</p></li>" +
                    "<li><p>Item 2.\n</p></li>" +
                    "<li><p>Item 3.\n</p></li>" +
                "</ol>" +
            "</div>"
        }
        
        val actual = <div>{ KnockOff.convert( source ).get }</div>.toString
        
        assertEquals( actual, expected )
    }
    
    
    def nested {
        
        val source = {
            "* Tab\n" +
            "\t* Tab\n" +
            "\t\t* Tab\n"
        }
        
        val expected = {
            "<div>" +
                "<ul>" +
                    "<li>Tab\n" +
                        "<ul>" +
                            "<li>Tab\n"
                                "<ul>" +
                                    "<li>Tab</li>" +
                                "</ul>" +
                            "</li>" +
                        "</ul>" +
                    "</li>" +
                "</ul>" +
            "</div>"
        }
        
        val actual = <div>{ KnockOff.convert( source ).get }</div>.toString
        
        assertEquals( actual, expected )
    }
    
    
    def nestedAnother {
        
        val source = {
            "1. First\n" +
            "2. Second:\n" +
            "\t* Fee\n" +
            "\t* Fie\n" +
            "\t* Foe\n" +
            "3. Third\n"
        }
        
        val expected = {
            <ol>
            <li>First</li>
            <li>Second:
            <ul>
            <li>Fee</li>
            <li>Fie</li>
            <li>Foe</li>
            </ul></li>
            <li>Third</li>
            </ol>
        }.toString
        
        val actual = <div>{ KnockOff.convert( source ).get }</div>.toString

        assertEquals( actual, expected )
    }
    
    
    def nestedParas {
     
        val source = {
            "1. First\n\n" +
            "2. Second:\n" +
            "\t* Fee\n" +
            "\t* Fie\n" +
            "\t* Foe\n\n" +
            "3. Third\n"
        }
        
        val expected = {
            <ol>
            <li>First</li>
            <li>Second:
            <ul>
            <li>Fee</li>
            <li>Fie</li>
            <li>Foe</li>
            </ul></li>
            <li>Third</li>
            </ol>
        }.toString
        
        val actual = <div>{ KnockOff.convert( source ).get }</div>.toString

        assertEquals( actual, expected )
    }
}