package knockoff2

import scala.util.parsing.combinator.RegexParsers

class ChunkParser extends RegexParsers with StringExtras {
    
  override def skipWhitespace = false
  
  def chunk : Parser[ Chunk ] = {
    horizontalRule | bulletLead | numberedLead | indentedChunk | 
    header | blockquote | textBlock | emptyLines
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
  
  /**
    Match a single line that is likely a bullet item.
  */
  def bulletLead : Parser[ Chunk ] = {
    """[ ]{0,3}[*\-+][\t ]+""".r ~> textLine ^^ { textChunk =>
      BulletLineChunk( textChunk.content )
    }
  }
  
  def numberedLead : Parser[ Chunk ] = {
    """[ ]{0,3}\d+\.[\t ]+""".r ~> textLine ^^ { textChunk =>
      NumberedLineChunk( textChunk.content )
    }
  }
  
  def header : Parser[ Chunk ] =
    ( setextHeaderEquals | setextHeaderDashes | atxHeader )

  def setextHeaderEquals : Parser[ Chunk ] =
    textLine <~ equalsLine ^^ ( s => HeaderChunk( 1, s.content ) )

  def setextHeaderDashes : Parser[ Chunk ] =
    textLine <~ dashesLine ^^ ( s => HeaderChunk( 2, s.content ) )

  def equalsLine : Parser[Any] = """=+\n?""".r

  def dashesLine : Parser[Any] = """-+\n?""".r

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

    
  
  
  // Utility Methods
  
  /** Take a series of very similar chunks and group them. */
  private def foldedString( texts : List[ Chunk ] ) : String =
    ( "" /: texts )( (current, text) => current + text.content )
}
