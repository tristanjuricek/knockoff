# Markdown Reader #

The [Markdown][] reader uses a single parser combinator to generate the parse tree.
The parser combinator is called the `MarkdownParser`, and is created by the
`createMarkdownParser` factory method.

Things to do:

1. Develop the list writer
1. Build a couple of spanning elements
2. Get headers rolling
3. Get a list mechanism rolling



## Should I build a scanner? ##

Both my system and the pandoc system tend to do some text munging that eventually
ends up in the [object model][]. So in a sense, the `MarkdownParser` is about all
the real scanner action we need. There will always have to be some flexibility
because of the hybrid nature of Markdown; generally speaking, the reader should try
to work around errors then say "uh oh your markdown is imperfect" - but we may want
a warning system.

[object model]: ../10-Object_Model.html


## `MarkdownMarkupReader` ##

**TODO** I'm not convinced I need to follow the pandoc system of a first pass that
only finds `LinkDefinition` objects. 


    // In com/tristanhunt/knockoff2/MarkdownReader.scala
    package com.tristanhunt.knockoff2

    import scala.util.parsing.combinator.{ RegexParsers }
    import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }

    abstract class MarkdownReader(
                      val markdownParser : MarkdownParser = new MarkdownParser )
    extends        LoggedParser {

      def readMarkdown( reader : Reader[Char] ) : Stream[Block] = {
        if ( reader.atEnd ) return Stream.empty
        
        import markdownParser._
        parse( markdown, reader ) match {
          case Error( msg, next ) =>
            logParsingError( msg, next )
            Stream.cons( PlainText( next.first.toString, next.first.toString, reader.pos ),
                         readMarkdown( next.rest ) )
          case Failure( msg, next ) =>
            logParsingError( msg, next )
            Stream.empty
          case Success( result, next ) =>
            Stream.cons( result, readMarkdown( next ) )
        }
      }
      
      def readMarkdown( str : String ) : Stream[Block] =
        readMarkdown( new CharSequenceReader( str ) )    
    }



## `MarkdownParser` ##

This markdown parser uses the parser combinator framework to generate a series of
basic definitions that often require a bit of look ahead. This really means we 
don't always use the typical literal or regular expressions, except in a few
cases. The framework is then used to indicate the processing order.


    // In com/tristanhunt/knockoff2/MarkdownParser.scala
    package com.tristanhunt.knockoff2
    // See the MarkdownParser imports
    
    class MarkdownParser extends RegexParsers {
      
      // If you don't do this, the regex parsers will drop leading whitespace
      override def skipWhitespace = false
      
      def markdown : Parser[Block] =
        linkDefinition | paragraph | blankLine
      
      def paragraph : Parser[PlainText] =
        rep1( plainText | nonEndNewline ) <~ paragraphEnd ^^ { seq =>
          val source = ( "" /: seq.map(_.text) )( _ + _ )
          PlainText( MarkdownString.unescape( source ), source, seq.head.pos )
        }
      
      // See the markdown plainText parser
    
      // See the markdown paragraphEnd parser
      
      // See the markdown nonEndNewline parser
      
      // See the markdown linkDefinition parser
      
      // See the markdown leadingWhitespace parser
      
      // See the markdown balancedInlineBrackets parser
      
      // See the markdown validURL parser
      
      // See the markdown linkTitle parser
      
      def blankLine : Parser[EmptyBlock] = {
        leadingWhitespace ~ "\n" ^^ {
          case leading ~ nl => EmptyBlock( leading.str + nl, leading.pos )
        }
      }      
    }
    // The MarkdownParser imports
    
    import com.tristanhunt.knockoff2.MarkdownString._
    import java.net.{ URI, URISyntaxException }
    import scala.collection.mutable.{ ListBuffer }
    import scala.util.parsing.combinator.{ RegexParsers }
    import scala.util.parsing.input.{ Reader }



## Parsing the `paragraphEnd` ##

The typical paragraph ends at:

1. A newline where the next line is a blank line, or the end of the document.
2. The end of the document.
3. A newline preceded by two spaces.

Thus, this has a 1 line "lookahead" for the first case.

    // The markdown paragraphEnd parser
    def paragraphEnd = new Parser[BlockEnd] {

      def apply( start : Reader[Char] ) = {
        
        def atEnd( reader : Reader[Char], cur : ListBuffer[Char] )
                 : ParseResult[BlockEnd] = {
          if ( reader.atEnd ) {
            // locate first new line
            val break = cur.takeWhile( _ != '\n' )
            val remaining = cur.drop( break.length )
            if ( !remaining.isEmpty && remaining.head == '\n' )
              break += '\n'
            Success( BlockEnd( break.mkString, start.pos ), reader )
          } else {
            reader.first match {
              case ' '  => atEnd( reader.rest, cur += reader.first )
              case '\t' => atEnd( reader.rest, cur += reader.first )
              case '\n' =>
                cur += '\n'
                val break = cur.takeWhile( _ != '\n' )
                if ( break.count( _ == ' ') > 1 )
                  Success( BlockEnd( (break += '\n').mkString, start.pos ), reader.rest )
                else {
                  if ( cur.count( _ == '\n' ) > 1 ) {
                    break += '\n'
                    var rem = start
                    ( 0 until break.length ).foreach { _ => rem = rem.rest }
                    Success( BlockEnd( break.mkString, start.pos ), rem )
                  } else {
                    atEnd( reader.rest, cur )
                  }
                }

              case _ => Failure( "Non-empty line after newline", start )
            }
          }
        }
        
        atEnd( start, new ListBuffer )
      }
    }

    // The markdown paragraphEnd parser tests
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

Some newlines are allowed...

    // The markdown nonEndNewline parser
    def nonEndNewline = new Parser[PlainText] {
      
      def apply( reader : Reader[Char] ) = {
        if ( reader.atEnd || reader.first != '\n' )
          Failure( "Not a newline", reader )
        else
          nextLine( reader.rest, new ListBuffer ) match {
            case None => Failure( "Ending newline", reader )
            case Some( line ) =>
              if ( line.trim == "" ) Failure( "Breaking newline", reader )
              else {
                var next = reader
                line.foreach { ch => next = reader.rest }
                Success( PlainText( line, line, reader.pos ), next )
              }
          }
      }
      
      def nextLine( reader : Reader[Char], cur : ListBuffer[Char] ) : Option[String] = {
        if ( reader.atEnd ) return None
        cur += reader.first
        if ( reader.first == '\n' ) return Some( cur.mkString )
        nextLine( reader.rest, cur )
      }
    }


## Parsing Plain Text ##

**TODO** plainText also needs to __not__ be several things. Basically, as we
recognize spanning elements, they need to all test against being in the middle of
random text...

**TODO** The remainingLine is only going to capture until the end of the line, which
means that plainText will at most capture up to 1 line's content. Can I safely not
do this?

    // The markdown plainText parser
    def plainText : Parser[PlainText] = new Parser[PlainText] {
      
      def apply( reader : Reader[Char] ) = {
        readUntilNewline( reader, new ListBuffer )
          .map { case ((line, rem)) =>
            Success( PlainText( unescape(line), line, reader.pos ), rem ) }
          .getOrElse( Failure( "No blank plainText", reader ) )
      }
      
      def readUntilNewline( reader : Reader[Char], cur : ListBuffer[Char] )
                  : Option[ (String, Reader[Char]) ] = {
        if ( reader.atEnd || reader.first == '\n' ) {
          if ( cur.mkString("").trim.isEmpty )
            None
          else {
            if ( !reader.atEnd ) cur += reader.first
            Some( (cur.mkString, reader.rest) )
          }
        } else {
          readUntilNewline( reader.rest, cur += reader.first )
        }
      }
    }
    
Possible unit tests:
    
    // The markdown plainText parser tests
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



## Parsing Link Definitions ##

__TODO__: Do we allow newlines in link definitions? (Where?)

    // The markdown linkDefinition parser
    def linkDefinition : Parser[LinkDefinition] = {
      ( leadingWhitespace ~ balancedInlineBrackets ~ "[ ]*:[ ]*".r ~
        validURL ~ "[ ]*".r ~ linkTitle ~ "[ ]*\\n?".r ) ^^ {

        case leading ~ brackets ~ sep ~ uri ~ ws ~ title ~ nl =>
          val id = brackets.trimEnds.unescape
          val sb = new StringBuilder( leading.str )
          sb.append( brackets ).append( sep ).append( uri.toString )
            .append( ws ).append( title.map(_.original).getOrElse("") )
            .append( nl )
          LinkDefinition( uri, id, title.map(_.text), sb.toString, leading.pos )
      }
    }

    // The markdown linkDefinition parser tests
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


## Parsing Balanced Brackets `[`, `]` ##

These are used to find links like `[a link][]`. Note that they are balanced, so that
sequences like `[some [thing]][ref]` would have a link that matches the text `some
[thing]`.

I'm using a custom `Parser` instance here because bracket matching isn't really
obvious using regular expression definitions. If you start with the left bracket `[`
you need to scan until you match the `]`. With a backtracking parser, you may be
able to go backwards, which means you *might* be able to specifically tag each `[`
or `]` character you find. Instead of scans, it's a simple lookup. My sense is that
this is a cheap ass speedup... maybe.
     
    // The markdown balancedInlineBrackets parser
    // Should match any inline sequences within a balanced bracket sequence.
    def balancedInlineBrackets = new Parser[String] {
      def apply( reader : Reader[Char] ) = {
        reader.first match {
          case '[' => matchClose( reader.first :: Nil, reader.rest, 1 )
          case _ => Failure( "Does not start with '['", reader )
        }
      }
      
      def matchClose( cur : List[Char], reader : Reader[Char], open : Int )
                    : ParseResult[String] = { // Return type needed here (recursive)
        if ( reader.atEnd )
          Failure( "No matching bracket, open: " + open, reader.rest )
        else {
          val newCur = reader.first :: cur
          reader.first match {
            case '[' =>
              matchClose( newCur, reader.rest, open + 1 )
            case ']' =>
              if ( cur.head == '\\' )
                return matchClose( newCur, reader.rest, open )
              if ( open > 1 )
                matchClose( newCur, reader.rest, open - 1 )
              else
                Success( newCur.reverseIterator.mkString, reader.rest )
            case _ =>
              matchClose( newCur, reader.rest, open )
          }
        }
      }
    }

A few basic tests are here mostly to make sure that I'm leveraging the basics of
anonymous `Parser` instances.

    // The balancedInlineBrackets test
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



## Parsing Valid URLs ##

I'm letting `java.net.URI` do the parsing, then return the parsed object. We'll
start with a scan identifying all legal characters of a URI, and stop when we hit
something like a space or a double-quote. Note that this has a few extensions to
the RFC, like assuming UTF-8, which I consider to be generally a Good Thing.

Note that this is going to try to skip past any bad URIs. It should be incredibly
rare, though possible, to pass a legal URI that's not whitespace separated but will
not be parsed here. The quick fix: separate with whitespace, dammit.

    // The markdown validURL parser
    def validURL = new Parser[URI] {
      
      def apply( reader : Reader[Char] ) = {
        val legal = legalURIString( reader, Nil )
        if ( legal.isEmpty )
          Failure( "URI is empty or starts with illegal characters", reader )
        else
          try {
            Success( new URI( legal ), reader.drop( legal.size ) )
          } catch {
            case ex : URISyntaxException => Failure( ex.getMessage, reader )
          }
      }
      
      // TODO - I wrote this in an airport, may want to review the comparison later
      // just in case, you know?
      val legalURLChars = "ABCDEFGHIJKLMOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" +
                          "0123456789_-!.~'()*,;:$&+=%?/[]@"
      
      // compare all code points: A-Z a-z 0-9 _-!.~'()*,;:$&+= % ?/[]@
      def legalURIString( reader : Reader[Char], cur : List[Char] ) : String = {
        if ( reader.atEnd ) return cur.reverse.mkString("")
        import java.lang.Character.digit
        val point = digit( reader.first, 10 )
        if ( legalURLChars contains reader.first )
          legalURIString( reader.rest, reader.first :: cur )
        else
          cur.reverse.mkString("")
      }
    }

Here are some basic unit tests for the markdown validURL parser.

    // The markdown validURL parser tests
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



## Parsing Link Titles ##

The LinkTitle can be quoted in a few different ways:

    "A simple title"
    "An \"escaped\" sequence"
    'Single quoted title'
    (A "title" that can have 'both' other quotes inside)

    // The markdown linkTitle parser
    def linkTitle = new Parser[ Option[LinkTitle] ] {

      def apply( reader : Reader[Char] ) : ParseResult[ Option[LinkTitle] ] = {
        val (opt, rest) = reader.first match {
          case '\'' => readToClose( '\'', reader.rest, List( reader.first ), false )
          case '"'  => readToClose( '"',  reader.rest, List( reader.first ), false )
          case '('  => readToClose( ')',  reader.rest, List( reader.first ), false )
          case _    => (None, reader)
        }
        opt match {
          case Some(title) => Success( Some(title), rest )
          case None        => Success( None, reader )
        }
      }
      
      def readToClose( ch : Char, reader : Reader[Char], cur : List[Char],
                       bs : Boolean )
                     : ( Option[LinkTitle], Reader[Char] ) = {
        if ( reader.atEnd ) return (None, reader)
        val newCur = reader.first :: cur
        if ( reader.first == ch ) {
          if ( bs ) return readToClose( ch, reader.rest, newCur, false )
          val original = newCur.reverse.mkString("")
          return ( Some( LinkTitle( original, reader.pos, original.trimEnds.unescape ) ),
                   reader.rest )
        }
        readToClose( ch, reader.rest, newCur, reader.first == '\\' )
      }
    }

Verification:

    // The markdown linkTitle parser tests
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



## Parsing Leading Whitespace ##

Typical leading whitespace for things like paragraphs, first-level URL definitions,
etc, basically can have up to 3 spaces. After 3 spaces, we consider things
_indented_ which makes them code blocks.

    // The markdown leadingWhitespace parser
    def leadingWhitespace = new Parser[StringPositional] {
      
      def apply( reader : Reader[Char] ) = checkWhitespace( reader, "", 0 )
      
      def checkWhitespace( reader : Reader[Char], cur : String, count : Int )
                         : ParseResult[StringPositional] = {
        if ( count > 3 )
          Failure( "Only 0-3 leading whitespace characters allowed", reader )
        else
          reader.first match {
            case ' ' => checkWhitespace( reader.rest, " " + cur, count + 1 )
            case _ => Success( StringPositional( cur ).setPos( reader.pos ),
                               reader )
          }
      }
    }

Verification:

    // The markdown leadingWhitespace parser tests
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
    
    


## Intermediate Representations ##

### `LinkTitle`

**TODO** This may only be used in markdown documents.

    // In com/tristanhunt/knockoff2/LinkTitle.scala
    package com.tristanhunt.knockoff2
    
    import scala.util.parsing.input.{ Position }
    
    case class LinkTitle( val original : String, val pos : Position,
                          val text : String )


## Specialized String Methods ##

### Escaping and unescaping text

Several characters have special meaning in a markdown document, and these can then
be escaped in order to avoid their special meaning with a backslash `\` character.

    // In com/tristanhunt/knockoff2/MarkdownString.scala
    package com.tristanhunt.knockoff2
    
    class MarkdownString( wrapped : String ) {
     
      /** Safely slices off the starting and leading characters of the string. */
      def trimEnds : String = {
        val start = 1
        val end = wrapped.length - 1
        if ( end < start ) wrapped else wrapped.slice( start, end )
      }
      
      def unescape : String = MarkdownString.unescape( wrapped )
    }
    
    object MarkdownString {
     
      implicit def MakeMarkdownString( wrapped : String ) =
        new MarkdownString( wrapped )
      
      val escapeableChars = "\\`*_{}[]()#+-.!>\'\""

      def unescape( source : String ) : String = {
        var buf : String = source
        for ( (escaped, unescaped) <-
                escapeableChars.map( ch => ("\\" + ch, ch.toString) ) )
          buf = buf.replace( escaped, unescaped )
        buf
      }
    }

Tests for all methods included

    // The MarkdownString tests
    import MarkdownString._

    it("should safely enable string.trimEnds") {
      "%foo%".trimEnds should equal ("foo")
      "1".trimEnds should equal ("1")
      "11".trimEnds should equal ("")
    }
    
    it("should replace all backslashes in escapeable content") {
      """Plus \+""".unescape should equal("Plus +")
      """Backslash \\""".unescape should equal("Backslash \\")
    }



## The Unit Test Suites ##

    // In test com/tristanhunt/knockoff2/MarkdownMarkupParserTestSuite.scala
    package com.tristanhunt.knockoff2
    
    // See the ScalaTest import
    
    class MarkdownMarkupParserTestSuite extends Spec with ShouldMatchers {
      
      val parser = new MarkdownParser
      import parser._
      
      describe("MarkdownMarkupReader") {
        // See the markdown linkDefinition parser tests
        // See the balancedInlineBrackets test
        // See the markdown validURL parser tests
        // See the markdown linkTitle parser tests
        // See the markdown leadingWhitespace parser tests
        // See the markdown paragraphEnd parser tests
        // See the markdown plainText parser tests
      }
    }
    
    // In test com/tristanhunt/knockoff2/MarkdownStringSuite.scala
    package com.tristanhunt.knockoff2
    
    // See the ScalaTest import
    import MarkdownString._
    
    class MarkdownStringSuite extends Spec with ShouldMatchers {
      
      describe("MarkdownString") {        
        // See the MarkdownString tests
      }
    }
    
    // The ScalaTest import
    import org.scalatest._
    import org.scalatest.matchers._    
    