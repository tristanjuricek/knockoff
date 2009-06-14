package com.tristanhunt.knockoff

import scala.util.parsing.combinator._

/**
 * A Markdown-like parser implementation that returns an object model capable of being easily 
 * translated into, well, some kind of markup document.
 * 
 * ## How to use this ##
 *
 * 1. `import` the Imports object, not this.
 * 2. Call `KnockOff.parse` on your `String` content.
 * 3. Use the `toXML` to convert to XHTML.
 *
 * A brief example:
 *
 *     import com.tristanhunt.knockoff.Imports._
 *
 *     val xhtml = KnockOff.parse( markdownString ).toXML
 *
 * ## Manipulating the object model ##
 *
 * With the above import, you can call `toXML` on the entire block list, the block, or the span,
 * which is a list of nads (really).
 *
 * So, for example, to take your first header, and grab the title:
 *
 *     val title = KnockOff.parse( markdownString ).find match {
 *         case Some( header : Header ) => header.nads.toXML.text
 *         case None =>
 *
 * ## The Unclean Design Approach of KnockOff ##
 *
 * The overall process is a little twacked, because I switch from using parser combinators to 
 * identify the blocks to using basically Regexes to figure out all the spanning elements. Hey, this
 * is an early-in-my-Scala-knowledge take on a project.
 *
 * So, the parsing stages can be seen as:
 *
 * 1. Interpret into MkBlock elements, which are an intermediate stage. This is done by the
 *    `MkBlockParser`, which is why this object is an `MkBlockParserFactory`.
 * 2. Convert the content of each MkBlock element into it's spanning nads.
 * 3.
 * 
 * @author Tristan Juricek <mr.tristan@gmail.com>
 */
object KnockOff {

    import com.tristanhunt.knockoff.other.FancyStrings.caseInsensitiveOrder
    import scala.collection.immutable._
    
    /**
     * A variation on the Option class that indicates an error message when there is a problem.
     */
    sealed abstract class KnockOffResult {
        def get : List[ Block ]
        def getOrElse[ B >: List[ Block ] ]( default: => B ) : B
        def isEmpty = true
    }
    
    case class Parsed( blocks : List[ Block ] ) extends KnockOffResult {
        def get = blocks
        def getOrElse[ B >: List[ Block ] ]( default : => B ) : B = blocks
        override def isEmpty = blocks.isEmpty
    }
    
    case class Failed( message : String ) extends KnockOffResult {
        def get = throw new Predef.NoSuchElementException
        def getOrElse[ B >: List[ Block ] ]( default : => B ) : B = default
    }
      
    /**
     * Parse a full markdown document. Returns the content as a list of Blocks, and a map of the
     * different link references.
     *
     * ## Internal Notes ##
     *
     * A bunch of extra hacks were placed in here to keep the MkBlock parsing step simple, but
     * fix some whitespace handling.
     */
    def parse( inputSource : String ) : KnockOffResult = {
        
        import MkBlockParser._
        
        // Replace tabs with 4 spaces.
        var normalized = inputSource.replace("\t", "    ")
        
        // Add extra lines after things that look a lot like headers.
        
        // ATX headers
        normalized = """(?m)^(#{1,6}[^\n]*#{1,6})$""".r.replaceAllIn( normalized, "$1\n" )
        
        // Setext headers
        normalized = """(?m)^([-=]{3,})\s*$""".r.replaceAllIn( normalized, "$1\n" )
        
        // When people have a line that *looks* like an empty code line, but is before a non-code
        // block line, well, they really don't mean it is.
        
        normalized = """[ ]{4}[ \t]*\n(\S)""".r.replaceAllIn( normalized, "\n$1" )
        
        // Do combinator parsing run, which will break down the markdown into "blocks".
        
        val mkblks : List[ MkBlock ] = MkBlockParser.parse( normalized ) match {
            case Success( blocks, _ ) => blocks
            case nope : NoSuccess => return Failed( nope.msg )
        }
        
        // Prepare a separate map of each link definition, which will be the second return value,
        // but also used during parsing to indicate if the link reference is valid or not.
        
        implicit var definitions : SortedMap[ String, LinkDefinition ] =
            TreeMap.empty( caseInsensitiveOrder )

        for ( defn <- mkblks.filter( _linkDefinitionCheck ) ) {
            
            definitions = definitions ++ {
                defn match {
                    case MkLinkDefinition( id, url, title ) =>
                        TreeMap( ( id, LinkDefinition( id, url, title ) ) )( caseInsensitiveOrder )

                    case list:MkLinkDefinitionList => _convertMkLinkDefinitionList( list )
                }
            }
        }

            
        // Perform MkBlock -> Block mapping
        //
        // TODO Get rid of the use of implicits for the SpanParser.

        implicit val parser = SpanParser( definitions )

        val blocks:List[Block] = mkblks.filter(
            blk => ! _linkDefinitionCheck( blk )
        ).map( _convert ).toList

        Parsed( blocks )
    }

    private def _convert( mkblk : MkBlock )( implicit parser : SpanParser ) : Block = {

        if ( mkblk.isInstanceOf[ MarkdownList ] )
            _convertMarkdownList( mkblk.asInstanceOf[ MarkdownList ] )
        else if ( mkblk.isInstanceOf[ ComplexMarkdownList ] )
            _convertComplexMarkdownList( mkblk.asInstanceOf[ ComplexMarkdownList ] )
        else 
            _convertMkBlock(mkblk)
    }
    
    private def _linkDefinitionCheck(blk:MkBlock):Boolean =
        blk.isInstanceOf[MkLinkDefinition] || blk.isInstanceOf[MkLinkDefinitionList]
    
    private def _convertMkLinkDefinitionList(list:MkLinkDefinitionList):
        SortedMap[String, LinkDefinition] = {
    
        var defs:SortedMap[String, LinkDefinition] = TreeMap.empty(caseInsensitiveOrder)
    
        list.definitions.foreach(defn => {
            defs = defs ++
                TreeMap((defn.id, LinkDefinition(defn.id, defn.url, defn.title)))(caseInsensitiveOrder)
        })
        
        defs
    }
    
    private def _convertMkBlock( mkblk:MkBlock )( implicit parser:SpanParser ):Block = mkblk match {

        case EmptySpace(_) => null

        case MkParagraph(markdown) => Paragraph(parser.parse(markdown))
    
        case MkHeader(markdown, level) =>
            Header(parser.parse(markdown), level)
    
        // TODO I probably want a special block parser that handles just grabbing the MkBlock
        // sequence, but doing a different mapping at this stage.
        //
        // This should always parse to something. Do I want to change the exception when it doesn't?
        case blockquote:MkBlockquote => Blockquote( parse( blockquote.markdown ).get )
    
        case HTMLMkBlock(html) => HTMLBlock(HTML(html))
    
        case kode:CodeMkBlock => CodeBlock(Text(kode.preformatted))
    
        case MkHorizontalRule(_) => HorizontalRule()
    }
    
    private def _convertMarkdownList(
            list    : MarkdownList
        ) ( implicit
            parser  : SpanParser
        ) : BlockList = {

        val blocks = list.items.map( _mapListItem )

        list match {

            case BulletListMkBlock( _ )    => UnorderedBlockList ( blocks )
            case NumberedListMkBlock( _ )  => OrderedBlockList   ( blocks )
        }
    }
    
    private def _convertComplexMarkdownList(
            complexList : ComplexMarkdownList
        ) ( implicit
            parser      : SpanParser
        ):ComplexBlockList = {
     
        val blocks = complexList.items.map( mkBlocks => mkBlocks.map( _convert ) )
        
        return complexList match {
            case bulleted:ComplexBulletListMkBlock => UnorderedComplexBlockList( blocks )
            case numbered:ComplexNumberedListMkBlock => OrderedComplexBlockList( blocks )
        }
    }
    
    private def _mapListItem(markdown:String)(implicit parser:SpanParser):Block = {
        val nads = parser.parse(markdown)
        if (nads.length == 1 && nads.first.isInstanceOf[HTML])
            HTMLBlock(nads.first.asInstanceOf[HTML])
        else
            Paragraph(nads)
    }
}