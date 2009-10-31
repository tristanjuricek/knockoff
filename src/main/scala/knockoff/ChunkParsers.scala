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
    textLine <~ equalsLine ^^ ( s => HeaderChunk( 1, s.content.trim ) )

  def setextHeaderDashes : Parser[ Chunk ] =
    textLine <~ dashesLine ^^ ( s => HeaderChunk( 2, s.content.trim ) )

  def equalsLine : Parser[Any] = """=+\n""".r

  def dashesLine : Parser[Any] = """-+\n""".r

  def atxHeader : Parser[ Chunk ] = {
    """#+ .*\n?""".r ^^ ( s =>
      HeaderChunk( s.countLeading('#'), s.trim('#').trim )
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
