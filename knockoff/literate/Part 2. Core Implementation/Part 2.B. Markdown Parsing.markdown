# Part 2.B. Markdown Parsing #

Parsing is done in three steps:

1. Chunking - The document is converted to a series of Chunk objects, each
eventually mapping to a block. This is kicked off by the `ChunkStreamFactory`. A
`Chunk` is generally a "block-level" element, but the final determination of what
is a block level element isn't complete until step 3.

2. Spanning - The spans of each chunk are identified.

3. Object model creation.

Note that things like block quotes and more complex lists turn into "documents
within documents".

### A Bit Of History To Satisfy Myself

In my first attempt, I tried building one big parser combinator, and then, slowly,
some part of my brain fell down a well. So that's why there's those steps 2 and 3,
it's not because I'm super smart, it's because it got the job done.


## Chunking ##

Breaks the markdown document into a Stream of `Chunk`s, so that later recognition
can function. This means this

* Identifies the major boundaries of block elments
* Figures out the `LinkDefinition`s. Those are needed for Span recognition.

When we run into something we can't parse, there's a simple rule; go on. If I detect
that there will be more and more problems, well. Hm.

Notably, this remembers the position of each chunk in the input.

### The Chunk Stream Factory

The whole process is wrapped by a "factory" which mostly handles continuing past any
errors in the document if possible. Errors are logged and we move ahead.

    // The ChunkStreamFactory
    trait ChunkStreamFactory extends Logged {

      /** Overridable factory method. */
      def newChunkParser : ChunkParser = new ChunkParser

      lazy val chunkParser : ChunkParser = newChunkParser

      def createChunkStream( str : String ) : Stream[(Chunk, Position)] =
        createChunkStream( new CharSequenceReader( str, 0 ) )
      
      def createChunkStream( reader : Reader[Char] ) : Stream[(Chunk, Position)] = {
        if ( reader.atEnd ) return Stream.empty
        chunkParser.parse( chunkParser.chunk, reader ) match {
          case chunkParser.Error( msg, next ) => {
            log( msg )
            log( "next == reader : " + (next == reader) )
            createChunkStream( next )
          }
          case chunkParser.Failure( msg, next ) => {
            log( msg )
            log( "next == reader : " + (next == reader) )
            createChunkStream( next )
          }
          case chunkParser.Success( result, next ) => {
            Stream.cons( ( result, reader.pos ), createChunkStream( next ) )
          }
        }
      }
    }

# The Chunk Parsers #

Mostly, this is a series of regular expressions built to find the next chunk in a
markdown document. 

    // The ChunkParser
    class ChunkParser extends RegexParsers with StringExtras {
        
      override def skipWhitespace = false
      
      def chunk : Parser[ Chunk ] = {
        horizontalRule | leadingStrongTextBlock | leadingEmTextBlock | bulletItem |
        numberedItem | indentedChunk | header | blockquote | linkDefinition |
        textBlock | emptyLines
      }
      
      def emptyLines : Parser[ Chunk ] =
        rep1( emptyLine ) ^^ ( str => EmptySpace( foldedString( str ) ) )
      
      def emptyLine : Parser[ Chunk ] =
        """[\t ]*\r?\n""".r ^^ ( str => EmptySpace( str ) )

      def textBlock : Parser[ Chunk ] =
        rep1( textLine ) ^^ { seq => TextChunk( foldedString(seq) ) }
      
      /** Match any line up until it ends with a newline. */
      def textLine : Parser[ Chunk ] =
        """[\t ]*\S[^\n]*\n?""".r ^^ { str => TextChunk(str) }
      
      def bulletItem : Parser[ Chunk ] =
        bulletLead ~ rep( trailingLine ) ^^ {
          case ~(lead, texts) => BulletLineChunk( foldedString( lead :: texts ) ) }
      
      /** Match a single line that is likely a bullet item. */
      def bulletLead : Parser[ Chunk ] =
        // """[ ]{0,3}[*\-+](\t|[ ]{0,4})""".r ~> not("[*\\-+]".r) ~> textLine ^^ {
        """[ ]{0,3}[*\-+](\t|[ ]{0,4})""".r ~> textLine ^^ {
          textChunk => BulletLineChunk( textChunk.content ) }
      
      /** A special case where an emphasis marker, using an asterix, on the first word
          in a text block doesn't make the block a list item. We'll only catch lines
          here that have an even number of asterixes, because if it's odd, well, you
          probably have an asterix line indicator followed by an emphasis. */
      def leadingEmTextBlock : Parser[ Chunk ] =
        """[ ]{0,3}\*""".r ~ notEvenAsterixes ~ rep( textLine ) ^^ {
          case ~(~(emLine, s), textSeq) => TextChunk( emLine + s + foldedString(textSeq) ) }
      
      def notEvenAsterixes = new Parser[String] {
    
        def apply( in : Reader[Char] ) : ParseResult[String] = {
          val (line, asterixCount, remaining) = readLine( in, new StringBuilder, 0 )
          if ( asterixCount >= 1 && asterixCount % 2 == 1 ) return Success( line, remaining )
          else Failure( "Odd number of asterixes, skipping.", in )
        }
        
        def readLine( in : Reader[Char], sb : StringBuilder, count : Int )
                    : (String, Int, Reader[Char]) = {
          if ( ! in.atEnd ) sb.append( in.first )
          if ( in.atEnd || in.first == '\n' ) return (sb.toString, count, in.rest)
          if ( in.first == '*' ) readLine( in.rest, sb, count + 1 )
          else readLine( in.rest, sb, count )
        }
      }
          
      /** A special case where an emphasis marker on a word on a text block doesn't
          make the block a list item. */
      def leadingStrongTextBlock : Parser[ Chunk ] =
        """[ ]{0,3}\*\*[^*\n]+\*\*[^\n]*\n?""".r ~ rep( textLine ) ^^ {
          case ~(strLine, textSeq) => TextChunk( strLine + foldedString(textSeq) ) }
      
      def numberedItem : Parser[ Chunk ] =
        numberedLead ~ rep( trailingLine ) ^^ {
          case ~(lead, texts) => NumberedLineChunk( foldedString( lead :: texts )) }
      
      def numberedLead : Parser[ Chunk ] =
        """[ ]{0,3}\d+\.(\t|[ ]{0,4})""".r ~> textLine ^^ {
          textChunk => NumberedLineChunk( textChunk.content ) }
      
      def trailingLine : Parser[ Chunk ] =
        """\t|[ ]{0,4}""".r ~> """[\S&&[^*\-+]&&[^\d]][^\n]*\n?""".r ^^ (
          s => TextChunk(s) )
      
      def header : Parser[ Chunk ] =
        ( setextHeaderEquals | setextHeaderDashes | atxHeader )

      def setextHeaderEquals : Parser[ Chunk ] =
        textLine <~ equalsLine ^^ ( s => HeaderChunk( 1, s.content.trim ) )

      def setextHeaderDashes : Parser[ Chunk ] =
        textLine <~ dashesLine ^^ ( s => HeaderChunk( 2, s.content.trim ) )

      def equalsLine : Parser[Any] = """=+\n""".r

      def dashesLine : Parser[Any] = """-+\n""".r

      def atxHeader : Parser[ Chunk ] =
        """#+ .*\n?""".r ^^ (
          s => HeaderChunk( s.countLeading('#'), s.trimChars('#').trim ) )
      
      def horizontalRule : Parser[ Chunk ] =
        """[ ]{0,3}[*\-_][\t ]?[*\-_][\t ]?[*\-_][\t *\-_]*\r?\n""".r ^^ {
          s => HorizontalRuleChunk }
      
      def indentedChunk : Parser[ Chunk ] =
        rep1( indentedLine ) ^^ ( lines => IndentedChunk( foldedString( lines ) ) )
      
      def indentedLine : Parser[ Chunk ] =
        """\t|[ ]{4}""".r ~> ( textLine | emptyLine | emptyString )

      def emptyString : Parser[ Chunk ] = "".r ^^ ( s => EmptySpace(s) )
      
      def blockquote : Parser[ Chunk ] =
        blockquotedLine ~ rep( blockquotedLine | textLine ) ^^ {
          case ~(lead, trailing) =>
            BlockquotedChunk( foldedString( lead :: trailing ) ) }
      
      def blockquotedLine : Parser[ Chunk ] =
        """^>[\t ]?""".r ~> ( textLine | emptyLine )
    
      def linkDefinition : Parser[ Chunk ] =
        linkIDAndURL ~ opt( linkTitle ) <~ """[ ]*\r?\n?""".r ^^ {
          case ~( idAndURL, titleOpt ) =>
            LinkDefinitionChunk( idAndURL._1, idAndURL._2, titleOpt ) }

      private def linkIDAndURL : Parser[ (String, String) ] =
        """[ ]{0,3}\[[^\[\]]*\]:[ ]+<?[\w\p{Punct}]+>?""".r ^^ { linkString =>
          val linkMatch = """^\[([^\[\]]+)\]:[ ]+<?([\w\p{Punct}]+)>?$""".r
                            .findFirstMatchIn( linkString.trim ).get;
          ( linkMatch.group(1), linkMatch.group(2) )
        }

      private def linkTitle : Parser[ String ] =
        """\s*""".r ~> """["'(].*["')]""".r ^^ ( // " <- My TextMate bundle fails here
          str => str.substring( 1, str.length - 1 ) )
      
      // Utility Methods
      
      /** Take a series of very similar chunks and group them. */
      private def foldedString( texts : List[ Chunk ] ) : String =
        ( "" /: texts )( (current, text) => current + text.content )
    }

### `ChunkParsersSpec`

    // In test com/tristanhunt/knockoff/ChunkParsersSpec.scala
    package com.tristanhunt.knockoff
    
    import org.scalatest._
    import org.scalatest.matchers._

    class ChunkParsersSpec extends ChunkParser with Spec with ShouldMatchers {

      describe("ChunkParser") {
        it("should handle simple bullet items") {
          val src = "* item 1\n* item 2\n"
          parse( chunk, src ).get should equal ( BulletLineChunk("item 1\n") )
        }
        
        it("should group a second line that's not a bullet") {
          val src = "*   item 1\n    more\n"
          parse( chunk, src ).get should equal (
            BulletLineChunk("item 1\nmore\n")
          )
        }
        
        it("should ignore whitespace around headers") {
          val src = "# Header 1 #"
          parse( chunk, src ).get should equal { HeaderChunk(1, "Header 1") }
        }
        
        it("should be ok with empty code blocks") {
          val src = "    "
          parse( chunk, src ).get should equal { IndentedChunk("") }
        }
        
        it("should not explode on a code block with a trailing line") {
          val line = "    line\n    "
          parse( chunk, line ).get should equal { IndentedChunk("line\n") }
        }
        
        it("should handle nothin' but code") {
          val src = "    This is just a code block.\n" +
                    "    \n" +
                    "    And it has a trailing whitespace line... that's also indented.\n" +
                    "    "
          parse( chunk, src ).get should equal { IndentedChunk(
            "This is just a code block.\n" +
            "\n" +
            "And it has a trailing whitespace line... that's also indented.\n"
          ) }
        }
        
        it("should deal with a CRLF") {
          val src = "This is a line \r\nthat is broken in\r\n a couple places.\r\n"
          parse( chunk, src ).get should equal { TextChunk(src) }
        }
        
        it("should deal with just a CRLF") {
          val src = "\u000d\u000a"
          parse( chunk, src ).get should equal { EmptySpace(src) }
        }
      }
    }

### The Chunks

Chunks are used to capture the major blocks in the early stage, and then once we've
grabbed the spanning elements of each block, to construct the final `Block` model.

    // The Chunk
    trait Chunk {
      def content : String
      def isLinkDefinition = false

      /** Create the Block and append to the list. */
      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter )
    }
    
### Text Chunk

Here is where I can apply hard breaks in the middle of paragraphs. If we've
recognized a `Text` span that contains two spaces and a newline, we split the span
sequence at this point into two lists, and then append two blocks. One of them will
be an `HTMLSpan(<br/>)`.

    // The TextChunk
    case class TextChunk( val content : String ) extends Chunk {

      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {

        val split = splitAtHardBreak( spans, new ListBuffer )
        list += Paragraph( split, position )
      }

      def splitAtHardBreak( spans : Seq[Span], cur : Buffer[Span] )
                          : Seq[Span] = {
        if ( spans.isEmpty ) return cur
        spans.first match {
          case text : Text =>
            // Skip past whitespace in the case we have some HTML.
            var start = 0
            if ( ! cur.isEmpty && cur.last.isInstanceOf[HTMLSpan] )
              while ( start < text.content.length &&
                      Character.isWhitespace( text.content(start) ) )
                start = start + 1
            text.content.indexOf("  \n", start) match {
              case -1 => {}
              case idx =>
                val end = idx + "  \n".length
                val (c1, c2) = ( text.content.substring( 0, idx ),
                                 text.content.substring( end ) )
                cur += Text(c1)
                cur += HTMLSpan("<br/>\n")
                return splitAtHardBreak( List(Text(c2)) ++ spans.drop(1), cur )
            }
          case _ => {}
        }
        return splitAtHardBreak( spans.drop(1), cur + spans.first )
      }
    }

### Horizontal Rule Chunk

    // The HorizontalRuleChunk
    case object HorizontalRuleChunk extends Chunk {
      val content = "* * *\n"
      
      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        list += HorizontalRule( position )
      }
    }

### Empty Space Chunk

Empty space only matters in cases where the lines are indented, which is a way of
dealing with editors that like to do things like strip out whitespace at the end of
a line.

This does not cover forced line brakes.

    // The EmptySpace
    case class EmptySpace( val content : String ) extends Chunk {

      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        if ( remaining.isEmpty ) return
        if ( list.isEmpty ) return
        list.last match {
          case lastCB : CodeBlock =>
            remaining.head._1 match {
              case ice : IndentedChunk =>
                list.update( list.length - 1,
                             CodeBlock( Text( lastCB.text.content + "\n" ),
                                        lastCB.position ) )
              case _ => {}
            }
          case _ => {}
        }
      }
    }

### Bullet Line Chunk

    // The BulletLineChunk
    case class BulletLineChunk( val content : String ) extends Chunk {

      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        val li = UnorderedItem( List(Paragraph(spans, position)), position )
        if ( list.isEmpty ) {
          list += UnorderedList( List(li) )
        } else {
          list.last match {
            case ul : UnorderedList =>
              list.update( list.length - 1, UnorderedList( ul.items ++ List(li) ) )
            case _ => list += UnorderedList( List(li) )
          }
        }
      }
    }

### Numbered Line Chunk

    // The NumberedLineChunk
    case class NumberedLineChunk( val content : String ) extends Chunk {

      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        val li = OrderedItem( List(Paragraph(spans, position)), position )
        if ( list.isEmpty ) {
          list += OrderedList( List(li) )
        } else {
          list.last match {
            case ol : OrderedList =>
              list.update( list.length - 1, OrderedList( ol.items ++ List(li) ) )
            case _ => list += OrderedList( List(li) )
          }
        }
      }
    }

### Header Chunk

    // The HeaderChunk
    case class HeaderChunk( val level : Int, val content : String ) extends Chunk {

      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        list += Header( level, spans, position )
      }
    }

### Indented Chunk

This represents a group of lines that have at least 4 spaces or 1 tab preceding the
line.

If the block before is a list, we append this to the end of that list. Otherwise,
append it as a new code block. Two code blocks will get combined here (because it's
common to have an empty line not be indented in many editors). Appending to the end
of a list means that we strip out the first indent and reparse things.

    // The IndentedChunk
    case class IndentedChunk( val content : String ) extends Chunk {

      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        if ( list.isEmpty ) {
          spans.first match {
            case text : Text => list += CodeBlock( text, position )
          }
        } else {
          list.last match {
            case OrderedList( items ) =>
              val blocks = discounter.knockoff( content )
              val li = OrderedItem( items.last.children ++ blocks, items.last.position )
              list.update( list.length - 1, OrderedList( items.take( items.length - 1) ++ List(li) ) )

            case UnorderedList( items ) =>
              val blocks = discounter.knockoff( content )
              val li =
                UnorderedItem( items.last.children ++ blocks, items.last.position )
              list.update( list.length - 1, UnorderedList( items.take( items.length - 1) ++ List(li) ) )

            case CodeBlock( text, position ) =>
              spans.first match {
                case next : Text =>
                  list.update( list.length - 1,
                               CodeBlock(Text(text.content + next.content), position) )
              }
            
            case _ =>
              spans.first match {
                case text : Text => list += CodeBlock( text, position )
              }
          }
        }
      }
    }

### Blockquoted Chunk

Represents a single level of blockquoted material. That means that it could also
contain content, which is then reparsed, recursively.

    // The BlockquotedChunk
    case class BlockquotedChunk( content : String ) extends Chunk {

      /** @param content The material, not parsed, but also not containing this
                         level's '>' characters. */
      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        val blocks = discounter.knockoff( content )
        list += Blockquote( blocks, position )
      }
    }

### Link Definition Chunk

    // The LinkDefinitionChunk
    case class LinkDefinitionChunk( val id : String, val url : String,
                                    val title : Option[ String ] )
    extends Chunk {

      override def isLinkDefinition = true
      
      def content : String =
        "[" + id + "]: " + url + title.map( " \"" + _ + "\"" ).getOrElse("")
      
      def appendNewBlock( list : ListBuffer[Block],
                          remaining : List[ (Chunk, Seq[Span], Position) ],
                          spans : Seq[Span], position : Position,
                          discounter : Discounter ) {
        list += LinkDefinition( id, url, title, position )
      }
    }



## Spanning ##

Our little spanning matching system is broken up into a tail-recursive system that
slowly puts together our strings by:

1. Trying out all alternatives of the next significant spanning element from the
current point.

2. Picking the best match based on earliest first location.

3. Processing current content if it can.

4. Processing the rest of the tail content.

### Span Converter

The converter implements the tail-recursive methods for spinning through the
content. Note that this recurses in two directions. One, when we find the next
spanning element, this will call itself to work on the tail, iterating "down" the
string. But on certain elements, the element itself contains a Span, so this
converter configures that matcher to kick off another parsing run on the substring
of that span.

    // The SpanConverter
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

### Span Match

The primary result returned by a `SpanMatcher`. It's `index` will become an ordering
attribute for determining the "best" match.

    // The SpanMatch
    case class SpanMatch( index : Int, before : Option[Text], current : Span,
                          after : Option[ String ] )

### Emphasis Matching

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

### Strong Matching

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

### Code Matching

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

### Link Matching

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

### Delimited Matcher

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

### Escaping

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

### SpanConverterSpec.scala

Most of the tests are next to the piece they verify above.

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



### MarkdownParsing.scala

    // In com/tristanhunt/knockoff/MarkdownParsing.scala
    package com.tristanhunt.knockoff
    
    import scala.util.parsing.combinator.Parsers
    import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
    import scala.util.logging.Logged
    
    // See the ChunkStreamFactory
    
    import scala.util.parsing.combinator.RegexParsers
    
    // See the ChunkParser
    
    import scala.collection.mutable.{ Buffer, ListBuffer }
    
    // See the Chunk
    
    // See the BlockquotedChunk
    
    // See the EmptySpace
    
    // See the HeaderChunk
    
    // See the HorizontalRuleChunk
    
    // See the IndentedChunk
    
    // See the LinkDefinitionChunk
    
    // See the NumberedLineChunk
    
    // See the TextChunk
    
    // See the BulletLineChunk
    
    import scala.util.matching.Regex.Match
    
    // See the SpanConverter