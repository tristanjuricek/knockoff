package com.tristanhunt.knockoff2

import org.scalatest._
import org.scalatest.matchers._    

class MarkdownMarkupParserTestSuite extends Spec with ShouldMatchers {
  
  val parser = new MarkdownParser
  import parser._
  
  describe("MarkdownMarkupReader") {
    it( "should grab a link definition without a title" ) {
      parse( linkDefinition, " [The ID]: http://example.com\nline1" ) match {
        case Success( link, rest ) =>
          link.id should equal( "The ID" )
          link.uri.toString should equal( "http://example.com" )
          link.title should equal ( None )
          link.source should equal ( " [The ID]: http://example.com\n" )
        
        case _ => fail( "unexpected result" )
      }
    }
    
    it( "should parse a link definition with a title" ) {
      parse( linkDefinition, "[id]: http://example.com (Title)" ) match {
        case Success( link, rest ) =>
          link.id should equal( "id" )
          link.uri.toString should equal( "http://example.com" )
          link.title should equal( Some("Title") )
          link.source should equal( "[id]: http://example.com (Title)" )
          
        case _ => fail( "unexpected result" )
      }
    }
    it("should match balancedInlineBrackets for links") {
      parse( balancedInlineBrackets, "[link]: reference" ) match {
        case Success( braces, next ) =>
          braces should equal ("[link]")
          next.first should equal (':')
        case _ => fail( "did not parse properly" )
      }
    }
    
    it("should make sure balancedInlineBrackets skips open phrases") {
      parse( balancedInlineBrackets, "[foo " ) match {
        case Failure( msg, pos ) =>
          msg should equal ("No matching bracket, open: 1")
        case _ => fail("unexpected result")
      }
    }
    
    it("should ignore escaped brackets") {
      parse( balancedInlineBrackets, "[foo in\\] bar]" ) match {
        case Success( braces, next ) =>
          braces should equal("[foo in\\] bar]")
        case _ => fail("unexpected result")
      }
    }
    it("should parse a normal URL") {
      parse( validURL, "http://example.com/path?foo=bar\" stuff" ) match {
        case Success( uri, next ) =>
          uri.toString should equal ("http://example.com/path?foo=bar")
          next.first should equal ('"')
        case _ => fail( "Did not parse properly" )
      }
    }
    
    it("should ignore an invalid URL with valid characters") {
      parse( validURL, " [ invalid ] " ) match {
        case Failure( msg, pos ) =>
          msg should startWith ("URI is empty or starts with illegal characters")
        case _ => fail( "Unexpected result" )
      }
    }
    
    it("should let validURL pass back the space after the URL") {
      parse( validURL, "http://example.com " ) match {
        case Success( uri, next ) =>
          uri.toString should equal( "http://example.com" )
          next.first should equal( ' ' )
        case _ => fail( "Unexpected result" )
      }
    }
    it("should find link titles for quotes") {
      parse( linkTitle, """'A \'Title\' with "quotes".'!""" ) match {
        case Success( title, rest ) =>
          title.get.original should equal( """'A \'Title\' with "quotes".'""" )
          title.get.text should equal( """A 'Title' with "quotes".""" )
        case _ => fail("unexpected result")
      }
    }
    
    it("should find link titles for double quotes") {
      parse( linkTitle, "\"A \\\"Title\\\" with 'double quotes'.\"!" ) match {
        case Success( title, rest ) =>
          title.get.original should equal( "\"A \\\"Title\\\" with 'double quotes'.\"" )
          title.get.text should equal( """A "Title" with 'double quotes'.""" )
        case _ => fail("unexpected result")
      }
    }
    
    it("should find link titles for parentheses") {
      parse( linkTitle, """(A \(Title\) with "parens".)!""" ) match {
        case Success( title, rest ) =>
          title.get.original should equal( """(A \(Title\) with "parens".)""" )
          title.get.text should equal( """A (Title) with "parens".""" )
        case _ => fail("unexpected result")
      }
    }
    it( "should parse leadingWhitespace before a non-whitespace character" ) {
      parse( leadingWhitespace, "a" ) match {
        case Success( strpos, rest ) =>
          strpos.str should equal( "" )
          rest.first should equal( 'a' )
        case _ => fail( "unexpected result" )
      }
      parse( leadingWhitespace, " b" ) match {
        case Success( strpos, rest ) =>
          strpos.str should equal( " " )
          rest.first should equal( 'b' )
        case _ => fail( "unexpected result" )
      }
      parse( leadingWhitespace, "  c" ) match {
        case Success( strpos, rest ) =>
          strpos.str should equal( "  " )          
        case _ => fail( "unexpected result" )
      }
      parse( leadingWhitespace, "   d" ) match {
        case Success( strpos, rest ) =>
          strpos.str should equal( "   " )
        case _ => fail( "unexpected result" )
      }
    }
    
    it("should not include 4 spaces as leadingWhitespace") {
      parse( leadingWhitespace, "    i" ) match {
        case Failure( msg, reader ) =>
          msg should equal( "Only 0-3 leading whitespace characters allowed" )
        case Success( strpos, _ ) => fail( "oops, got " + strpos )
        case _ => fail( "unexpected result" )
      }
    }
    it( "should see a paragraphEnd at the end of the document" ) {
      parse( paragraphEnd, "" ) match {
        case Success( endLine, rest ) =>
          endLine.source should equal( "" )
          rest.atEnd should equal( true )
        case _ => fail( "Unexpected result" )
      }
      
      parse( paragraphEnd, "\n" ) match {
        case Success( end, rest ) =>
          end.source should equal( "\n" )
          rest.atEnd should equal( true )
        case _ => fail( "Unexpected result" )
      }
    }
    
    it( "should see a paragraphEnd after two or more spaces" ) {
      parse( paragraphEnd, "   \nH" ) match {
        case Success( endLine, rest ) =>
          endLine.source should equal( "   \n" )
          rest.first should equal( 'H' )
        case _ => fail( "Unexpected result" )
      }
    }
    
    it( "should see a paragraphEnd before an emptyLine" ) {
      parse( paragraphEnd, " \n  \n" ) match {
        case Success( end, rest ) =>
          end.source should equal( " \n" )
          rest.first should equal( ' ' )
        case _ => fail( "Unexpected result" )
      }
    }
    it( "should parse paragraph with a paragraphEnd" ) {
      parse( paragraph, "para\n\n" ) match {
        case Success( text, reader ) =>
          text.source should equal( "para\n" )
          text.text should equal( "para\n" )
          reader.first should equal( '\n' )
        case _ => fail( "Unexpected result" )
      }
    }
    
    it ( "should parse plainText that finishes the document" ) {
      parse( paragraph, "para" ) match {
        case Success( text, reader ) =>
          text.source should equal( "para" )
          text.text should equal( "para" )
          reader.atEnd should equal( true )
        case _ => fail( "Unexpected result" )
      }
    }
    
    it ( "should see a normal newline as plainText" ) {
      parse( paragraph, "line 1\nline2\n" ) match {
        case Success( text, reader ) =>
          text.source should equal( "line 1\nline2 \n" )
          text.text should equal( "line 1\nline2 \n" )
          reader.atEnd should equal( true )
        case _ => fail( "Unexpected result" )
      }
    }
  }
}

