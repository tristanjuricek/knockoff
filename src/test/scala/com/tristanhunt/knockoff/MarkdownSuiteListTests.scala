package com.tristanhunt.knockoff

import org.scalatest.testng._
import org.testng.annotations._
import org.testng.Assert._
import scala.xml._

/**
 * OK, so I split out the "ordered and unordered lists" stuff from the normal markdown test suite
 * because I needed to figure out how to do this nested parsing thing.
 */
class MarkdownSuiteListTests extends TestNGSuite {

    import Imports._
    
    @Test
    def bulletsTight {
     
        val bullets = List( "*", "-", "+" )
     
        bullets.foreach ( bullet => {
     
            val source =
                bullet + " asterisk 1\n" +
                bullet + " asterisk 2\n" +
                bullet + " asterisk 3\n"

            val actual = <div>{ knockoff( source ).get.toXML }</div>.toString
        
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
    @Test
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

            val actual = <div>{ knockoff( source ).get.toXML }</div>.toString
        
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
    
    
    @Test
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
        
        val actualSingleSpaced = <div>{ knockoff( singleSpaced ).get.toXML }</div>.toString
        val actualDoubleSpaced = <div>{ knockoff( doubleSpaced ).get.toXML }</div>.toString
        
        assertEquals( actualSingleSpaced, expected )
        
        assertEquals( actualDoubleSpaced, expected )
    }
    
    
    @Test
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
        
        val actualWithSpaces    = <div>{ knockoff( looseWithSpaces ).get.toXML }</div>.toString
        val acutalWithTabs      = <div>{ knockoff( looseWithTabs ).get.toXML }</div>.toString
        
        assertEquals( actualWithSpaces, expected )
        
        assertEquals( acutalWithTabs, expected )
    }
    
    
    @Test
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
        
        val actual = <div>{ knockoff( source ).get.toXML }</div>.toString
        
        assertEquals( actual, expected )
    }
    
    
    @Test
    def nested {
        
        val source = {
            "* Tab\n" +
            "\t* Tab\n" +
            "\t\t* Tab\n"
        }
        
        val expected = {
            "<div>" +
                "<ul>" +
                    "<li><p>Tab\n</p>" +
                        "<ul>" +
                            "<li><p>Tab\n</p>" +
                                "<ul>" +
                                    "<li>Tab\n</li>" +
                                "</ul>" +
                            "</li>" +
                        "</ul>" +
                    "</li>" +
                "</ul>" +
            "</div>"
        }
        
        val actual = <div>{ knockoff( source ).get.toXML }</div>.toString
        
        assertEquals( actual, expected )
    }
    
    
    @Test
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
            "<div>" +
            "<ol>" +
                "<li><p>First\n</p></li>" +
                "<li>" +
                    "<p>Second:\n</p>" +
                    "<ul>" +
                    "<li>Fee\n</li>" +
                    "<li>Fie\n</li>" +
                    "<li>Foe\n</li>" +
                    "</ul>" +
                "</li>" +
                "<li><p>Third\n</p></li>" +
            "</ol>" +
            "</div>"
        }.toString
        
        val actual = <div>{ knockoff( source ).get.toXML }</div>.toString

        assertEquals( actual, expected )
    }
    
    
    @Test
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
            "<div>" +
            "<ol>" +
                "<li><p>First\n</p></li>" +
                "<li>" +
                    "<p>Second:\n</p>" +
                    "<ul>" +
                        "<li>Fee\n</li>" +
                        "<li>Fie\n</li>" +
                        "<li>Foe\n</li>" +
                    "</ul>" +
                "</li>" +
                "<li><p>Third\n</p></li>" +
            "</ol>" +
            "</div>"
        }.toString
        
        val actual = <div>{ knockoff( source ).get.toXML }</div>.toString

        assertEquals( actual, expected )
    }
}