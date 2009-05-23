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
       
        val mkBlockParser = new MkBlockParser
        
        val mkblks:List[MkBlock] = {
        
            mkBlockParser.parseAll( mkBlockParser.markdownDocument, src4 ) match {

                case mkBlockParser.Success(list, _) => _trim( _condenseSpacedLists( list, Nil ) )

                case fail:mkBlockParser.Failure => {

                    println( fail.toString )
                    return None
                }
            
                case _ => return None
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
    
    private def _convert(mkblk:MkBlock)(implicit parser:SpanParser):Block = {

        if (mkblk.isInstanceOf[MarkdownList])
            _convertMarkdownList(mkblk.asInstanceOf[MarkdownList])
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
    
    private def _convertMkBlock(mkblk:MkBlock)(implicit parser:SpanParser):Block = mkblk match {

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
    
    private def _convertMarkdownList(list:MarkdownList)(implicit parser:SpanParser):BlockList = {
        val blocks = list.markdownSeq.map(_mapListItem)
        list match {
            case BulletListMkBlock(_)       => UnorderedBlockList(blocks)
            case NumberedListMkBlock(_)     => OrderedBlockList(blocks)
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
    private def _trim(list:List[MkBlock]):List[MkBlock] = {
        
        def nonEmptyMkBlock(b:MkBlock):Boolean =
            b.isInstanceOf[MkParagraph] == false || b.asInstanceOf[MkParagraph].markdown.length > 0
    
        val start = list findIndexOf nonEmptyMkBlock
    
        val end = list.length - (list.reverse findIndexOf nonEmptyMkBlock)
        
        list.slice(start, end)
    }
    
    /**
     * Special rule to group any lists that were separated by whitespace, because it is an option.
     */
    private def _condenseSpacedLists(in:List[MkBlock], out:List[MkBlock]):List[MkBlock] = {
        
        if (in.isEmpty) {
            return out
        }
        
        if (in.head.isInstanceOf[MarkdownList] && !out.isEmpty && in.head.getClass == out.last.getClass) {
            val condensed = out.last match {
                case BulletListMkBlock(outItems)  => BulletListMkBlock(outItems ++ in.head.asInstanceOf[MarkdownList].items)
                case NumberedListMkBlock(outItems)        => NumberedListMkBlock(outItems ++ in.head.asInstanceOf[MarkdownList].items)
                case _ => error("Unknown MarkdownList Class: " + out.head.getClass)
            }
            return _condenseSpacedLists(in.tail, out.dropRight(1) + condensed)
        }
        
        return _condenseSpacedLists(in.tail, out + in.head)
    }
}