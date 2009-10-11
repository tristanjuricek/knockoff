## `ChunkParsers` ##

Mostly, this is a series of regular expressions built to find the next chunk in a
markdown document. Some expressions, like headings, will determine the "real span"
part separately from the expressions here.

All of the methods return a `Chunk` parser type, even when grouping the parsers
together. To group things together, the `foldedString` will combine

    // In knockoff/ChunkParsers.scala
    package knockoff
    
    import scala.util.parsing.combinator.RegexParsers

    class ChunkParser extends RegexParsers with StringExtras {
        
      override def skipWhitespace = false
      
      def chunk : Parser[ Chunk ] = {
        horizontalRule | bulletItem | numberedItem | indentedChunk |
        header | blockquote | linkDefinition | textBlock | emptyLines
      }
      
      def emptyLines : Parser[ Chunk ] =
        rep1( emptyLine ) ^^ ( str => EmptySpace( foldedString( str ) ) )
      
      def emptyLine : Parser[ Chunk ] =
        """[\t ]*\n""".r ^^ ( str => EmptySpace( str ) )

      def textBlock : Parser[ Chunk ] =
        rep1( textLine ) ^^ ( str => TextChunk( foldedString( str ) ) )

      /** Match any line up until it ends with a newline. */
      def textLine : Parser[ Chunk ] =
        """[\t ]*\S[^\n]*\n?""".r ^^ ( str => TextChunk( str ) )
      
      def bulletItem : Parser[ Chunk ] = {
        bulletLead ~ rep( trailingLine ) ^^ { case ~(lead, texts) =>
          BulletLineChunk( foldedString( lead :: texts ) )
        }
      }
      
      /**
        Match a single line that is likely a bullet item.
      */
      def bulletLead : Parser[ Chunk ] = {
        """[ ]{0,3}[*\-+](\t|[ ]{0,4})""".r ~> not("[*\\-+]".r) ~> textLine ^^ { textChunk =>
          BulletLineChunk( textChunk.content )
        }
      }
      
      def numberedItem : Parser[ Chunk ] = {
        numberedLead ~ rep( trailingLine ) ^^ { case ~(lead, texts) =>
          NumberedLineChunk( foldedString( lead :: texts ) )
        }
      }
      
      def numberedLead : Parser[ Chunk ] = {
        """[ ]{0,3}\d+\.(\t|[ ]{0,4})""".r ~> textLine ^^ { textChunk =>
          NumberedLineChunk( textChunk.content )
        }
      }
      
      def trailingLine : Parser[ Chunk ] = {
        """\t|[ ]{0,4}""".r ~> """[\S&&[^*\-+]&&[^\d]][^\n]*\n?""".r ^^ (
          s => TextChunk(s)
        )
      }
      
      def header : Parser[ Chunk ] =
        ( setextHeaderEquals | setextHeaderDashes | atxHeader )

      def setextHeaderEquals : Parser[ Chunk ] =
        textLine <~ equalsLine ^^ ( s => HeaderChunk( 1, s.content ) )

      def setextHeaderDashes : Parser[ Chunk ] =
        textLine <~ dashesLine ^^ ( s => HeaderChunk( 2, s.content ) )

      def equalsLine : Parser[Any] = """=+\n""".r

      def dashesLine : Parser[Any] = """-+\n""".r

      def atxHeader : Parser[ Chunk ] = {
        """#+ .*\n?""".r ^^ ( s =>
          HeaderChunk( s.countLeading('#'), s.trim('#') )
        )
      }
      
      def horizontalRule : Parser[ Chunk ] = {
        """[ ]{0,3}[*\-_][\t ]?[*\-_][\t ]?[*\-_][\t *\-_]*\n""".r ^^ {
          s => HorizontalRuleChunk
        }
      }
      
      def indentedChunk : Parser[ Chunk ] =
        rep1( indentedLine ) ^^ ( lines => IndentedChunk( foldedString( lines ) ) )
      
      def indentedLine : Parser[ Chunk ] =
        """\t|[ ]{4}""".r ~> ( textLine | emptyLine )
      
      def blockquote : Parser[ Chunk ] = {
        blockquotedLine ~ rep( blockquotedLine | textLine ) ^^ {
          case ~(lead, trailing) => BlockquotedChunk( foldedString( lead :: trailing ) )
        }
      }
      
      def blockquotedLine : Parser[ Chunk ] =
        """^>[\t ]?""".r ~> ( textLine | emptyLine )
    
      def linkDefinition : Parser[ Chunk ] = {
        linkIDAndURL ~ opt( linkTitle ) <~ """[ ]*\n?""".r ^^ {
          case ~( idAndURL, titleOpt ) =>
            LinkDefinitionChunk( idAndURL._1, idAndURL._2, titleOpt )
        }
      }

      private def linkIDAndURL : Parser[ (String, String) ] = {
        """[ ]{0,3}\[[^\[\]]*\]:[ ]+<?[\w\p{Punct}]+>?""".r ^^ (
          linkString => {
            val linkMatch = {
              """^\[([^\[\]]+)\]:[ ]+<?([\w\p{Punct}]+)>?$""".r.findFirstMatchIn(
                linkString.trim
              ).get
            }
            ( linkMatch.group(1), linkMatch.group(2) )
          }
        )
      }

      private def linkTitle : Parser[ String ] = {
        """\s*""".r ~> """["'(].*["')]""".r ^^ ( str =>
          str.substring( 1, str.length - 1 )
        )
      }
      
      
      // Utility Methods
      
      /** Take a series of very similar chunks and group them. */
      private def foldedString( texts : List[ Chunk ] ) : String =
        ( "" /: texts )( (current, text) => current + text.content )
    }


## `ChunkParsersSpec` ##

    // In test knockoff/ChunkParsersSpec.scala
    package knockoff
    
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
        }
    }