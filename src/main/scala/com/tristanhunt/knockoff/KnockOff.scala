package com.tristanhunt.knockoff

import util.parsing.combinator._

/**
 * A Markdown-like parser implementation that returns an object model capable of being easily 
 * translated into, well, some kind of markup document.
 * 
 * The overall process is a little twacked, because I switch from using parser combinators to 
 * identify the blocks to using basically Regexes to figure out all the spanning elements.
 * 
 * @author Tristan Juricek <mr.tristan@gmail.com>
 */
object KnockOff {
    
    import collection.immutable._
    import other.FancyStrings.caseInsensitiveOrder
 
    /**
     * Convert a full markdown document to a NodeBuffer that can be injected into another XML
     * document. Like a full page:
     *
     *     <html><head></head><body>{KnockOff.convert(markdown)}</body></html>
     *
     * TODO Describe output modification.
     */
    def convert(src:String):Option[xml.NodeBuffer] = {
        parse(src) match {
            case Some(blocks)    => Some(BlockConverter.toXML(blocks))
            case None            => None
        }
    }
    
      
    val mkBlockParser = new MkBlockParser
 
    /**
     * Parse a full markdown document. Returns the content as a list of Blocks, and a map of the
     * different link references.
     */
    def parse( src:String ):Option[ List[ Block ] ] = {
        
        // Replace tabs with 4 spaces.
        val src2 = src.replace("\t", "    ")
        
        // Add extra lines after things that look a lot like headers.
        
        // ATX headers
        val src3 = src2.replaceAll( """(?m)^(#{1,6}[^\n]*#{1,6})$""", "$1\n" )
        
        // Setext headers
        val src4 = src3.replaceAll( """(?m)^([-=]{3,})\s*$""", "$1\n" )
        
        // Do combinator parsing run, which will break down the markdown into "blocks".

        
        val mkblks:List[MkBlock] = {
            _parseMkBlocks( src4 ) match {
                case Some( blocks ) => blocks
                case None => return None
            }
        }
        
        // Prepare a separate map of each link definition, which will be the second return value,
        // but also used during parsing to indicate if the link reference is valid or not.
        
        implicit var definitions:SortedMap[String, LinkDefinition] = TreeMap.Empty(caseInsensitiveOrder)

        for (defn <- mkblks.filter(_linkDefinitionCheck)) {
            definitions = definitions ++ {
                defn match {
                    case MkLinkDefinition(id, url, title) =>
                        TreeMap((id, LinkDefinition(id, url, title)))(caseInsensitiveOrder)

                    case list:MkLinkDefinitionList => _convertMkLinkDefinitionList(list)
                }
            }
        }

            
        // Perform MkBlock -> Block mapping

        implicit val parser = SpanParser(definitions)
        val blocks:List[Block] = mkblks.filter(blk => !_linkDefinitionCheck(blk)).map(_convert).toList

        Some(blocks)
    }
    
    
    private def _parseMkBlocks( source : String ) : Option[ List[ MkBlock ] ] = {
        
        mkBlockParser.parseAll( mkBlockParser.markdownDocument, source ) match {

            case mkBlockParser.Success( list, _) =>
                return Some ( _trim(
                    _createComplexLists(
                        _condenseSpacedLists( list, Nil ),
                        Nil
                    )
                ) )

            case fail:mkBlockParser.Failure => {

                println( fail.toString )
                return None
            }
        
            case _ => return None
        }
    }
    
    private def _convert(mkblk:MkBlock)(implicit parser:SpanParser):Block = {

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
    
        var defs:SortedMap[String, LinkDefinition] = TreeMap.Empty(caseInsensitiveOrder)
    
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
        case blockquote:MkBlockquote => Blockquote(parse(blockquote.markdown).get)
    
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
    
    /**
     * Remove empty MkParagraphs from the beginning and end.
     */
    private def _trim( list : List[ MkBlock ] ) : List[ MkBlock ] = {
        
        def nonEmptyMkBlock( b : MkBlock ) : Boolean =
            b.isInstanceOf[MkParagraph] == false || b.asInstanceOf[MkParagraph].markdown.length > 0
    
        val start = list findIndexOf nonEmptyMkBlock
    
        val end = list.length - (list.reverse findIndexOf nonEmptyMkBlock)
        
        list.slice(start, end)
    }
    
    /**
     * Convert each of the list types into complex lists where:
     *
     * 1. There is a perceieved "code block" right after a list
     * 2. There is a \n[ ]4 sequence, which indicates that there was no empty line right after
     *    a sequence.
     *
     * To keep the implementation simple, we pass through the unordered lists first, then ordered.
     *
     * Note that this expects the lists themselves to be completely grouped. If we have a "code
     * block" in between two lists, however, we'll have to combine those things together.
     */
    private def _createComplexLists(
            in : List[ MkBlock ],
            out : List[ MkBlock ]
        ) : List[ MkBlock ] = {
       
       // TODO I've got to figure out a better way to keep this stuff organized.
        
        _createComplexListsFromEmbeddedNumberedLists(
            _createComplexListsFromEmbeddedBulletLists( in, out ),
            Nil
        )
    }
    
    /**
     * 
     */
    private def _createComplexListsFromEmbeddedBulletLists(
            in : List[ MkBlock ],
            out : List[ MkBlock ]
        ) : List[ MkBlock ] = {
    
        if ( in.isEmpty )
            return out
        
        if ( in.head.isInstanceOf[ BulletListMkBlock ] ) {
         
            val bulletList = in.head.asInstanceOf[ BulletListMkBlock ]

            if ( bulletList.items.find( _.contains( "\n     " ) ).isDefined ) {

                val complexList = ComplexBulletListMkBlock(
                    bulletList.items.map( _convertItemToBlocks )
                )
                
                return _createComplexListsFromEmbeddedBulletLists( in.tail, out + complexList )
            }
        }
        
        return _createComplexListsFromEmbeddedBulletLists( in.tail, out + in.head )
    }

    
    /**
     * Converts a simple list item to a MkBlock.
     *
     * 1. Remove a level of indentation.
     */
    private def _convertItemToBlocks( source:String ):List[ MkBlock ] = {
        
        var blocks = _createNextBlock( io.Source.fromString( source ).getLines.toList, Nil )
        
        blocks.flatMap( block => {
            if ( block.startsWith( "    " ) )
                _parseMkBlocks( block.substring( Math.min( 4, block.length ) ) ).get
            else
                _parseMkBlocks( block ).get
        } )
    }
    
    private def _createNextBlock( sources : List[ String ], blocks : List[ String ] ):List[ String ] = {
        
        if ( sources.isEmpty )
            return blocks
        
        if ( sources.head.startsWith( "    " ) && blocks.last.startsWith( "    " ) )
            return _createNextBlock( sources.tail, blocks.dropRight( 1 ) + ( blocks.last + sources.head ) )
        
        return _createNextBlock( sources.tail, blocks + sources.head )
    }
    
    
    /**
     * 
     */
    private def _createComplexListsFromEmbeddedNumberedLists(
            in : List[ MkBlock ],
            out : List[ MkBlock ]
        ) : List[ MkBlock ] = {
    
        if ( in.isEmpty )
            return out
        
        if ( in.head.isInstanceOf[ NumberedListMkBlock ] ) {
         
            val bulletList = in.head.asInstanceOf[ NumberedListMkBlock ]

            if ( bulletList.items.find( _.contains( "\n     " ) ).isDefined ) {

                val complexList = ComplexNumberedListMkBlock(
                    bulletList.items.map( _convertItemToBlocks )
                )
                
                return _createComplexListsFromEmbeddedNumberedLists( in.tail, out + complexList )
            }
        }
        
        return _createComplexListsFromEmbeddedNumberedLists( in.tail, out + in.head )
    }

    
    /**
     * Special rule to group any lists that were separated by whitespace, because it is an option.
     */
    private def _condenseSpacedLists(
            /** We consume this list and recurse until it's empty. */
            in  : List[ MkBlock ],
            /** Where we stick the output... which, by the way, will be pulled from when condensing. */
            out : List[ MkBlock ]
        ) : List[ MkBlock ] = {
        
        if (in.isEmpty) {
            return out
        }
        
        // First, look for the case where either are complex. If this is the case, when we condense
        // we'll always convert both lists to the complex case.
        //
        // TODO This looks like its time for a better generic class... brain... melting... whatever...
        
        if ( ! out.isEmpty ) {

            if (
                ( in.head.isInstanceOf[ NumberedListMkBlock ] ) &&
                ( out.last.isInstanceOf[ NumberedListMkBlock ] )
            ) {
                
                val right = in.head.asInstanceOf[ NumberedListMkBlock ]
                
                val left = out.last.asInstanceOf[ NumberedListMkBlock ]
                
                val condensed = NumberedListMkBlock( left.items ++ right.items )
                
                return _condenseSpacedLists( in.tail, out.dropRight(1) + condensed )
                
            } else if (
                ( in.head.isInstanceOf[ BulletListMkBlock ] ) &&
                ( out.last.isInstanceOf[ BulletListMkBlock ] )
            ) {
                
                val right = in.head.asInstanceOf[ BulletListMkBlock ]
                
                val left = out.last.asInstanceOf[ BulletListMkBlock ]
                
                val condensed = BulletListMkBlock( left.items ++ right.items )
                
                return _condenseSpacedLists( in.tail, out.dropRight(1) + condensed )
            }
            
        }
        
        // OK, we're not at a list, so just move the current item to the next list.
        
        return _condenseSpacedLists( in.tail, out + in.head )
    }
}