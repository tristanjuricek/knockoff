# Span Conversion #

Our little spanning matching system is broken up into a tail-recursive system that
slowly puts together our strings by:

1. Trying out all alternatives of the next significant spanning element from the
current point.

2. Picking the best match based on earliest first location.

3. Processing current content if it can.

4. Processing the rest of the tail content.


## `SpanConverter` ##

The converter implements the tail-recursive methods for spinning through the
content. Note that this recurses in two directions. One, when we find the next
spanning element, this will call itself to work on the tail, iterating "down" the
string. But on certain elements, the element itself contains a Span, so this
converter configures that matcher to kick off another parsing run on the substring
of that span.

    // In com/tristanhunt/knockoff/SpanConverter.scala
    package com.tristanhunt.knockoff
    
    import scala.util.matching.Regex.Match
    
    class SpanConverter( definitions : Seq[LinkDefinitionChunk] )
    extends Function1[ Chunk, Seq[Span] ] with StringExtras {

      // See the SpanMatch
    
      // See the DelimMatcher
    
      def apply( chunk : Chunk ) : Seq[Span] = {
        chunk match {
          case IndentedChunk( content ) => List( new Text(content) )
          case _ => convert( chunk.content, Nil )
        }
      }
      
      /** Tail-recursive method halts when the content argument is empty. */
      protected def convert( content : String, current : List[Span] ) : Seq[Span] = {

        if ( content.isEmpty ) return current

        val textOnly = SpanMatch( content.length, None, Text( content ), None )

        val best = ( textOnly /: matchers ){ (current, findMatch) =>
          findMatch( content ) match {
            case None => current
            case Some( nextMatch ) =>
              if ( nextMatch.index < current.index ) nextMatch
              else current
          }
        }
      
        val updated = current ::: best.before.toList ::: List( best.current )
      
        best.after match {
          case None              => updated
          case Some( remaining ) => convert( remaining, updated )
        }
      }
      
      def matchers : List[ String => Option[SpanMatch] ] = List(
        matchDoubleCodes, matchSingleCodes, findReferenceMatch, findAutomaticMatch,
        findNormalMatch, matchHTMLComment,
        matchEntity, matchHTMLSpan, matchUnderscoreStrongAndEm,
        matchAsterixStrongAndEm, matchUnderscoreStrong, matchAsterixStrong,
        matchUnderscoreEmphasis, matchAsterixEmphasis
      )
      
      // See the emphasis matchers
      
      // See the strong matchers
      
      // See the strong + emphasis matchers
      
      // See the code matchers
     
      // See the HTML matchers
      
      // See the link matchers
    }

### `SpanMatch`

The primary result returned by a `SpanMatcher`. It's `index` will become an ordering
attribute for determining the "best" match.

    // The SpanMatch
    case class SpanMatch( index : Int, before : Option[Text], current : Span,
                          after : Option[ String ] )

### `Emphasis` Matching

Emphasis can start with an underscore `_` or an asterix `*`, and can have embedded
elements. Examples:

    _dude_
    *dude*
    an _emphasized expression `with code` inside!_

Both are configured by the `EmphasisMatchers`.

    // The emphasis matchers
    val matchUnderscoreEmphasis =
      new DelimMatcher( "_", Emphasis(_), true, Some('\\') )

    val matchAsterixEmphasis =
      new DelimMatcher( "*", Emphasis(_), true, Some('\\') )

    // The emphasis matchers specification
    it("should match emphasis underscores containing asterix emphases") {
      val txt = "an _underscore *and block* em_"
      convert( txt ) should equal {
        List( Text("an "),
              Emphasis(
                List( Text("underscore "),
                      Emphasis( Text("and block") :: Nil ),
                      Text(" em") ) ) ) }
    }

### `Strong` (Like Bull) Matching

Like `Emphasis` elements, `Strong` elements use two underscores `__` or asterixes
`**` to figure themselves out.

    // The strong matchers
    val matchUnderscoreStrong =
        new DelimMatcher( "__", Strong(_), true, Some('\\') )
    
    val matchAsterixStrong =
        new DelimMatcher( "**", Strong(_), true, Some('\\') )

    // The strong matchers specification
    it("should match strong underscores containing asterix emphases") {
      val txt = "an __underscore **and asterix** strong__"
      convert( txt ) should equal {
        List( Text("an "),
              Strong( List( Text("underscore "),
                            Strong( List(Text("and asterix")) ),
                            Text(" strong") ) ) )
      }
    }

### Strong and `em` at the same time

    // The strong + emphasis matchers
    val matchUnderscoreStrongAndEm = 
      new DelimMatcher( "___", seq => Strong( List(Emphasis(seq)) ), true,
                        Some('\\') )

    val matchAsterixStrongAndEm =
      new DelimMatcher( "***", seq => Strong( List(Emphasis(seq)) ), true,
                        Some('\\') )

### `Code` Matching

Two varations of code blocks:

    A `normal code` block
    A ``code with a `backtick` inside``

This is all done by balanced code matching via the `EqualDelimiterMatcher`.

    // The code matchers
    val matchDoubleCodes =
      new DelimMatcher( "``", s => CodeSpan( s.first.asInstanceOf[Text].content ),
                        false, None )

    val matchSingleCodes = 
      new DelimMatcher( "`", s => CodeSpan( s.first.asInstanceOf[Text].content ),
                        false, None )
      
    // The code matchers specification
    it( "should parse a couple of single code blocks in text" ) {
      val txt = "a `code1` and a `code 2`"
      convert( txt ) should equal {
        List( Text("a "), CodeSpan("code1"), Text(" and a "), CodeSpan("code 2") ) }
    }

    it("should not care about other elements in the code") {
      val txt = "This `code block *with em*` is OK"
      convert( txt ) should equal {
        List( Text("This "), CodeSpan( "code block *with em*" ), Text(" is OK") ) }
    }
      
    it("double-tick code markers should preserve whitespace") {
      val txt = "AA `` ` `` BB"
      convert( txt ) should equal {
        List( Text("AA "), CodeSpan(" ` "), Text(" BB") ) }
    }

### HTML Matching

If we find any kind of HTML/XML-like element within the content, and it's not a
single element, we try to find the ending element. If that segment isn't
well-formed, we just ignore the element, and treat it like text.

Any sequences of HTML in content are matched by the `InlineHTMLMatcher`. Note that
this uses a recursive method `hasMatchedClose` to deal with the situations where
one span contains other spans - it's basically like parenthesis matching.

    // The HTML matchers
    private val startElement = """<[ ]*([a-zA-Z0-9:_]+)[ \t]*[^>]*?(/?+)>""".r
    
    def matchHTMLSpan( source : String ) : Option[SpanMatch] = {
      startElement.findFirstMatchIn( source ).map { open =>
        val hasEnd = open.group(2) == "/"
        val before = open.before.toOption.map( Text(_) )
        val noEnd = SpanMatch( open.start, before, HTMLSpan( open.matched ),
                               open.after.toOption )
        if ( ! hasEnd ) {
          hasMatchedClose( source, open.group(1), open.end, 1 ) match {
            case Some((close, after)) =>
              val before = open.before.toOption.map( Text(_) )
              val html = HTMLSpan( source.substring( open.start, close ) )
              SpanMatch( open.start, before, html, after.toOption )
            // Let no html-like thing go unpunished.
            case None => noEnd
          }
        } else {
          noEnd
        }
      }
    }
    
    private def hasMatchedClose( source : String, tag : String, from : Int,
                                 opens : Int )
                               : Option[ (Int, CharSequence) ] = {

      val opener = ("(?i)<[ ]*" + tag + "[ \t]*[^>]*?(/?+)*>").r
      val closer = ("(?i)</[ ]*" + tag + "[ ]*>").r
      
      val nextOpen  = opener.findFirstMatchIn( source.substring(from) )
      val nextClose = closer.findFirstMatchIn( source.substring(from) )

      if ( ! nextClose.isDefined ) return None
      
      if ( nextOpen.isDefined && ( nextOpen.get.start < nextClose.get.start ) ) {
        hasMatchedClose( source, tag, from + nextOpen.get.end, opens + 1 )
      } else if ( opens > 1 ) {
        hasMatchedClose( source, tag, from + nextClose.get.end, opens - 1 )
      } else {
        Some( ( from + nextClose.get.end, nextClose.get.after ) )
      }
    }
    
    private val matchEntityRE = """&\w+;""".r

    def matchEntity( source : String ) : Option[ SpanMatch ] =
      matchEntityRE.findFirstMatchIn( source ).map { entityMatch =>
        val before = entityMatch.before.toOption.map( Text(_) )
        val html = HTMLSpan( entityMatch.matched )
        SpanMatch( entityMatch.start, before, html, entityMatch.after.toOption )
      }

    def matchHTMLComment( source : String ) :Option[ SpanMatch ] = {
      val open = source.indexOf("<!--")
      if ( open > -1 ) {
        val close = source.indexOf( "-->", open )
        if ( close > -1 ) {
          val before = source.substring( 0, open ).toOption.map( Text(_) )
          val html = HTMLSpan( source.substring( open, close + "-->".length ) )
          val after = source.substring( close + "-->".length ).toOption
          return Some( SpanMatch( open, before, html, after ) )
        }
      }
      return None
    }

    // The HTML span matcher specification
    it("should find an <a> and an <img>") {
      val txt = """with <a href="http://example.com">a link</a> and an 
                  |<img src="foo.img"/> ha!""".stripMargin
      convert( txt ) should equal {
        List( Text("with "),
              HTMLSpan("""<a href="http://example.com">a link</a>"""),
              Text(" and an \n"), HTMLSpan("""<img src="foo.img"/>"""),
              Text(" ha!") ) }
    }
      
    it("should wrap a <span> that contains another <span>") {
      val txt = """a <span class="container">contains <span>something</span>
                  | else</span> without a problem <br /> !""".stripMargin
      convert( txt ) should equal {
        List( Text("a "),
              HTMLSpan( """<span class="container">contains """ +
                        "<span>something</span>\n else</span>" ),
              Text(" without a problem "), HTMLSpan("<br />"), Text(" !") ) }
    }
      
    it("should find a couple of entities and pass them through") {
      val txt = "an &amp; and an &em; are in here"
      convert( txt ) should equal {
        List( Text("an "), HTMLSpan("&amp;"), Text(" and an "), HTMLSpan("&em;"),
              Text(" are in here") ) }
    }
      
    it("should handle HTML headers defined in text") {
      val txt = "<h2 id=\"overview\">Overview</h2>"
      convert( txt ) should equal {
        List( HTMLSpan("<h2 id=\"overview\">Overview</h2>") ) }
    }



## Link Matching ##

Recall that links come in 4 major varieties, and here is where we figure out what
they are:

1. Inline urls: `[wrapped](url "title")`
2. Inline images: `![alt](url "title")`
3. Reference: `[wrapped][id]`
4. Automatic: `<url>`

A major caveat is that we need to be able to handle parens within the wrapped text.
So, things like:

    I'm [a link](http://example.com "Title (in paren)")

    // The link matchers
    private val automaticLinkRE = """<((http:|mailto:|https:)\S+)>""".r
    
    def findAutomaticMatch( source : String ) : Option[ SpanMatch ] =
      automaticLinkRE.findFirstMatchIn( source ).map { aMatch =>
        val url = aMatch.group(1)
        val before = aMatch.before.toOption.map( Text(_) )
        val link = Link( List( Text(url) ), url, None )
        SpanMatch( aMatch.start, before, link, aMatch.after.toOption )
      }
    
    def findNormalMatch( source : String ) : Option[SpanMatch] =
      normalLinks.findFirstMatchIn( source )
                 .flatMap { matchr => findNormalMatch( source, matchr ) }
    
    def findNormalMatch( source : String, matchr : Match ) : Option[ SpanMatch ] = {
      val isImage     = matchr.group(1) == "!" || matchr.group(4) == "!"
      val hasTitle    = matchr.group(7) != null
      val wrapped     = if( hasTitle ) matchr.group(5) else matchr.group(2)
      val url         = if( hasTitle ) matchr.group(6) else matchr.group(3)
      val titleOption = if ( hasTitle ) Some( matchr.group(7) ) else None
      val before = matchr.before.toOption.map( Text(_) )
      val link = if ( isImage ) ImageLink( convert(wrapped, Nil), url, titleOption )
                 else Link( convert(wrapped, Nil), url, titleOption )
      Some( SpanMatch( matchr.start, before, link, matchr.after.toOption ) )
    }
      
    // See the normalLinks regular expression
      
    /** We have to match parens, to support this stuff: [wr [app] ed] [thing] */
    def findReferenceMatch( source : String ) : Option[SpanMatch] = {
      val firstOpen = source.indexOf('[')
      if ( firstOpen == -1 ) return None
      
      val firstClose =
        source.findBalanced('[', ']', firstOpen).getOrElse( return None )

      val secondPart = source.substring( firstClose + 1 )

      val secondMatch =
        """^\s*(\[)""".r.findFirstMatchIn( secondPart ).getOrElse( return None )

      val secondClose =
        secondPart.findBalanced( '[', ']', secondMatch.start(1) ).get
      if ( secondClose == -1 ) return None

      val refID = {
        val no2 = secondPart.substring( secondMatch.start(1) + 1, secondClose )
        if ( no2.isEmpty ) source.substring( firstOpen + 1, firstClose ) else no2
      }
      val precedingText = source.substring( 0, firstOpen ).toOption.map( Text(_) )
      
      definitions.find( _.id equalsIgnoreCase refID ).map {
        definition : LinkDefinitionChunk =>
          val link = Link( List( Text(source.substring(firstOpen + 1, firstClose)) ),
                           definition.url, definition.title )
          val after = source.substring( firstClose + secondClose + 2 ).toOption
          SpanMatch( firstOpen, precedingText, link, after )
      }
    }
    
This regular expression will try to match links like: `[wrap](url)` and
`[wrap](url "title")`, in image mode or not.
        
Groups:
1. "!" for image, no title
2. wrapped content, no title
3. url, no title
4. "!" for image, with title
5. wrapped content, with title
6. url, with title
7. title

The definition:

    // The normalLinks regular expression
    val normalLinks =
      ( """(!?)\[([^\]]*)\][\t ]*\(<?([\S&&[^)>]]*)>?\)|""" +
        """(!?)\[([^\]]*)\][\t ]*\(<?([\S&&[^)>]]*)>?[\t ]+"([^)]*)"\)""" ).r

    // The link matching specification
    it("should discover inline, image, automatic, and reference links") {
      val defs = List( new LinkDefinitionChunk( "link1", "http://example.com",
                                                Some("title") ) )
      val txt = "A [link](http://example.com/link1) " +
                "An ![image link](http://example.com/image1 \"image test\") " +
                "The <http://example.com/automatic> A [reference link] [link1]"
      convert( txt, defs ) should equal {
        List( Text("A "),
              Link( List(Text("link")), "http://example.com/link1", None ),
          		Text(" An "),
              ImageLink( List(Text("image link")), "http://example.com/image1",
                         Some("image test") ),
              Text(" The "),
              Link( List(Text("http://example.com/automatic")),
                    "http://example.com/automatic", None ),
              Text(" A "),
              Link( List(Text("reference link")), "http://example.com",
                    Some("title") ) ) }
    }
      
    it("should hande link references in different case") {
      val defs = List( new LinkDefinitionChunk( "link 1", "http://example.com/1",
                                                None ),
                       new LinkDefinitionChunk( "link 2", "http://example.com/2",
                                                None ) )
      val txt = "[Link 1][] and [link 2][]"
      convert( txt, defs ) should equal {
        List( Link( List(Text("Link 1")), "http://example.com/1", None ),
              Text(" and "),
              Link( List(Text("link 2")), "http://example.com/2", None ) ) }
    }



## `DelimMatcher` ##

Many of the elements are delimited by the identical character sequence on either
side of the text. This does the dirty work of finding those matches, whatever that
character sequence may be.

    // The DelimMatcher
    
    /** @param delim The delimiter string to match the next 2 sequences of.
        @param toSpanMatch Factory to create the actual SpanMatch.
        @param recursive If you want the contained element to be reconverted.
        @param escape If set, how you can escape this sequence. */
    class DelimMatcher( delim : String, toSpan : Seq[Span] => Span,
                        recursive : Boolean, escape : Option[Char] )
    extends Function1[ String, Option[SpanMatch] ] {
      
      def apply( source : String ) : Option[SpanMatch] = {
        
        source.nextNIndicesOf( 2, delim, escape ) match {
          case List( start, end ) =>
            if ( start + delim.length >= end ) return None
            val contained = source.substring( start + delim.length, end )
            val content = if ( recursive ) convert( contained, Nil )
                          else List( Text(contained) )
            val before = source.substringOption( 0, start ).map( Text(_) )
            val after = source.substringOption( end + delim.length, source.length )
            val mapped = toSpan( content )
            Some( SpanMatch( start, before, mapped, after ) )
          case _ => None
        }
      }
    }



## Escaping ##

The following characters should be escapable, which means that by putting a
backslash `\` in front of the character, you should not see it in output:

    \   backslash
    `   backtick
    *   asterisk
    _   underscore
    {}  curly braces
    []  square brackets
    ()  parentheses
    #   hash mark
    +   plus sign
    -   minus sign (hyphen)
    .   dot
    !   exclamation mark

This means that a normal backslash by itself should not be seen unless it's inside
a code block.

    // The Escaping specification
    it("should escape asterixes in content") {
      val txt = """an \*escaped\* emphasis"""
      convert( txt ) should equal( List( Text("""an \*escaped\* emphasis""") ) )
    }
      
    it("should escape backticks in content") {
      val txt = """an escaped \' backtick"""
      convert( txt ) should equal( List( Text("""an escaped \' backtick""") ) )
    }
      
    it("should ignore backslashes in code") {
      val txt = """a backslash `\` in code"""
      convert( txt ) should equal(
        List( Text("""a backslash """), CodeSpan("\\"), Text(""" in code""") ) )
    }



## Testing Framework ##

    // In test com/tristanhunt/knockoff/SpanConverterSpec.scala
    package com.tristanhunt.knockoff

    import org.scalatest._
    import org.scalatest.matchers._
    import scala.util.parsing.input.NoPosition
    
    class SpanConverterSpec extends Spec with ShouldMatchers {

      def convert( txt : String ) : List[Span] = convert( txt, Nil )
      
      def convert( txt : String, defs : Seq[LinkDefinitionChunk] ) : List[Span] =
        new SpanConverter( defs )( TextChunk(txt) ).toList

      describe("SpanConverter") {
        // See the code matchers specification
      
        // See the emphasis matchers specification

        // See the strong matchers specification
      
        // See the HTML span matcher specification
      
        // See the link matching specification
      
        // See the Escaping specification
      }
    }