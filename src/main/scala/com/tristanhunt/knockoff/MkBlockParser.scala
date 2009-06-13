package com.tristanhunt.knockoff

import scala.util.parsing.combinator._

/**
 * Parses a complete markdown source into intermediate 'MkBlock' elements. Basically, identifies
 * all of the "block" elements before we parse into spanning elements. 
 *
 * Why does this only convert a portion of the blocks? Because the mixture of HTML and "normal"
 * non-HTML things made my brain sizzle.
 *
 * @author Tristan Juricek <mr.tristan@gmail.com>
 */
object MkBlockParser extends RegexParsers {
    
    import scala.collection.mutable.{ Buffer, ListBuffer }
        
    def parse( source : String ) : ParseResult[ List[ MkBlock ] ] = {
        
        parseAll( markdownDocument, source ) match {

            case Success( list, _ ) => {
                
                val trimmed = trim( condenseSpacedLists( list, new ListBuffer ) )

                val complexified = convertComplexLists( trimmed, Nil )
                
                Success( complexified, null )
            }

            case nope : NoSuccess => return nope
        }
    }
    
    
    // Parser Definition
    
    /**
     * Whitespace does make grouping significant in this system. This class should deal with all
     * significant whitespace, however, and pass it through properly.
     */
    override def skipWhitespace = false
    
    /**
     * A markdown document is a sequence of things separated by empty whitespace. Note that this
     * parser combinator expects that the input will not "ram" things together.
     */
    def markdownDocument : Parser[ List[ MkBlock ] ]  = repsep( thing, emptyLine )

    
    def thing : Parser[ MkBlock ] = (
    
        horizontalRule | listMkBlock | codeMkBlock | header | linkDefinition | htmlMkBlock |
        blockquote | textMkBlock
    
    ) ^^ ( blockElement => blockElement.asInstanceOf[ MkBlock ] )
    
    
    /**
     * Text lines need to retain their newlines. The user can use "hard" formatting with newlines,
     * and also indicate two spaces at the end of the line to force a paragraph break.
     */
    def textLine : Parser[ MkParagraph ] =
        """[ ]*\S[^\n]*\n?""".r ^^ ( str => MkParagraph( str ) )


    def textMkBlock : Parser[ MkParagraph ] =
        rep( textLine ) ^^ ( list => MkParagraph( _concatMkParagraphs( list ) ) )
    
    
    /**
     * We don't actually parse HTML, just assume the simple rule of "if it looks like markup, you
     * intended it to be markup". At least until there's some blank space.
     *
     * NOTE: We need to avoid the <http://fancy.link> automatic link interface here.
     */
    def htmlMkBlock : Parser[ HTMLMkBlock ] = {
        htmlLine ~ rep( textLine ) ^^ (
            paras => HTMLMkBlock( _concatMkParagraphs( paras._1 :: paras._2 ) )
        )
    }


    def htmlLine : Parser[ MkParagraph ] =
        """<[ ]*[\w="]*[ ]*\n?""".r ^^ ( str => MkParagraph( str ) )

    
    /**
     * For some reason, this groups bullet lists broken with an empty line into a single list...
     */
    def listMkBlock : Parser[ MkBlock ] =
        bulletList | numberedList


    def bulletList : Parser[ BulletListMkBlock ] =
        rep1( bulletItem ) ^^ ( list => BulletListMkBlock( list ) )


    def bulletItem : Parser[ String ] = {
        bulletLead ~ rep( noBulletLine | indentedLine ) ^^ (
            stringAndPara => stringAndPara._1 + _concatMkParagraphs( stringAndPara._2 )
        )
    }
    

    def bulletLead : Parser[ String ] = {
        """[ ]{0,3}[*\-+]\s+""".r ~> textLine ^^ ( para => para.markdown )
    }
    

    def noBulletLine : Parser[ MkParagraph ] =
        """[ ]{0,3}[\S&&[^*\-+]][^\n]*\n?""".r ^^ ( string => MkParagraph( string ) )
    
    
    def numberedList : Parser[ NumberedListMkBlock ] =
        rep1( numberedItem ) ^^ ( list => NumberedListMkBlock( list ) )


    def numberedItem : Parser[ String ] = {
        numberedLead ~ rep( noNumberLine | indentedLine ) ^^ (
            stringAndParas => stringAndParas._1 + _concatMkParagraphs( stringAndParas._2 )
        )
    }

        
    def numberedLead : Parser[ String ] =
        """[ ]{0,3}\d+\.\s+""".r ~> textLine ^^ ( para => para.markdown )

        
    def noNumberLine : Parser[ MkParagraph ] =
        not( numberedLead ) ~> textLine
    
    
    /**
     * One or more blank lines mostly is used to break blocks apart from each other in the list.
     * This should be generally passed over in blocks. Note that this should generally capture
     * more lines as a single empty space element.
     */
    def emptyLine : Parser[ EmptySpace ] =
        """\s*\n""".r ^^ (s => EmptySpace(s))
    
    
    def header : Parser[ MkHeader ] =
        ( setextHeader1 | setextHeader2 | atxHeader )
        

    def setextHeader1 : Parser[ MkHeader ] =
        textLine <~ equalsLine ^^ ( mkParagraph => MkHeader( mkParagraph.markdown.trim, 1 ) )


    def setextHeader2 : Parser[ MkHeader ] =
        textLine <~ dashLine ^^ ( mkParagraph => MkHeader( mkParagraph.markdown.trim, 2 ) )


    def equalsLine:Parser[Any]          = """=+\n?""".r


    def dashLine:Parser[Any]            = """-+\n?""".r


    def atxHeader : Parser[ MkHeader ] =
        """#+ .*\n?""".r       ^^ (str => _atxHeader(str.trim))
    
    
    def blockquote : Parser[ MkBlockquote ] =
        rep1(blockquotedLine) ^^ (list => MkBlockquote(_concatMkParagraphs(list)))

    
    def blockquotedLine : Parser[ MkParagraph ] =
        ">"~(textLine | emptyLine) ^^ (tup => MkParagraph(tup._1 + tup._2.markdown))

        
    def codeMkBlock : Parser[ CodeMkBlock ] =
        rep1( indentedLine ) ^^ (list => CodeMkBlock(_concatMkParagraphs(list)))

        
    def indentedLine : Parser[ MkParagraph ] =
        "    "~(textLine | emptyLine) ^^ (v => MkParagraph(v._1 + v._2.markdown))

        
    def horizontalRule : Parser[ MkHorizontalRule ] =
        """[ ]{0,3}[*\-_][ ]?[*\-_][ ]?[*\-_][ *\-_]*\n""".r ^^ (rule => MkHorizontalRule(rule))

    
    def linkDefinition : Parser[ MkLinkDefinitionList ] =
        rep1( linkDefinitionItem ) ^^ ( list => MkLinkDefinitionList( list ) )

    
    def linkDefinitionItem : Parser[ MkLinkDefinition ] = {

        linkBase ~ opt( linkTitle ) <~ """[ ]*\n?""".r ^^ (
            parser => parser._2 match {
                case Some(title)    => MkLinkDefinition(parser._1._1, parser._1._2, title)
                case None           => MkLinkDefinition(parser._1._1, parser._1._2, "")
            }
        )
    }


    def linkBase:Parser[ ( String, String ) ] =
        """[ ]{0,3}\[[^\[\]]*\]:[ ]+<?[\w\p{Punct}]+>?""".r ^^ (str => _readLinkBase(str))

    
    def linkTitle:Parser[ String ] =
        """\s*""".r ~> """["'(].*["')]""".r ^^ ( str => str.substring( 1, str.length - 1 ) )


    /**
     * Cheap way of grouping the linkBase regex result. I find this to be an ugly solution.
     */
    private def _readLinkBase( linkString:String ):( String,String ) = {

        val linkMatch = {
            """^\[([^\[\]]+)\]:[ ]+<?([\w\p{Punct}]+)>?$""".r.findFirstMatchIn(
                linkString.trim
            ).get
        }

        return ( linkMatch.group( 1 ), linkMatch.group( 2 ) )
    }
    
    
    /**
     * Combine hard-wrapped lines together into single blocks.
     */
    private def _concatMkParagraphs( list:List[ MkParagraph]  ):String = {

        val sb = new StringBuilder
        
        list.foreach( para => sb.append( para.markdown ) )

        return sb.toString
    }
    
    
    /**
     * Remove any leading and trailing # characters, but use the leading # character to determine 
     * level of the header.
     */
    private def _atxHeader( text:String ):MkHeader = {

        val content         = """^#+ (.*?)\s?#*+$""".r.replaceFirstIn( text, "$1" )
        val leadingHashes   = """^(#+) .*?\s?#*+$""".r.replaceFirstIn( text, "$1" )

        return MkHeader( content, leadingHashes.length )
    }
    
    
    // List Munging
    //
    // This complexity here is why I messed up in my basic approach.
    
    private class IntermediateParserString( string : String ) {
        
        def source = io.Source.fromString( string )
        
        def hasEmbeddedList : Boolean = hasEmbeddedList( 0 )
        
        def hasEmbeddedList( startingLine : Int ) : Boolean =
            source.getLines.toList.drop( startingLine ).exists( hasEmbeddedItem )

        private def hasEmbeddedItem( string : String ):Boolean =
            hasEmbeddedBullet( string ) || hasEmbeddedNumberList( string )

        def hasEmbeddedItem : Boolean = hasEmbeddedItem( string )

        private def hasEmbeddedBullet( str : String ) : Boolean =
            """[ ]{4,}[*\-+]\s+""".r.findFirstIn( str ).isDefined
    
        private def hasEmbeddedNumberList( str : String ) : Boolean =
            """[ ]{4,}\d+\.\s+""".r.findFirstIn( str ).isDefined
            
        def stripLeadingAndParse : List[ MkBlock ] = {
         
            val stripped = io.Source.fromString( string ).getLines.map(
                line => line.substring( Math.min( 4, line.length ) )
            ).mkString( "" )
            
            val parsed = parse( stripped ).getOrElse(
                error( "Unable to parse as a list: " + string )
            )
            
            parsed
        }
        
        def splitToMkBlockList : List[ MkBlock ] = {
            
            val lines = string.source.getLines.toList
            
            val splitPosition = lines.findIndexOf( line => line.hasEmbeddedItem )
            
            val ( normalLeading, embedded ) = lines.splitAt( splitPosition )

            MkParagraph( normalLeading.mkString( "" ) ) :: (
                embedded.mkString( "" ).stripLeadingAndParse
            )
        }
    }
    
    private implicit def IntermediateParserString( string : String ) : IntermediateParserString =
        new IntermediateParserString( string )
    
    
    private class IntermediateParserStringList( list : Seq[ String ] ) {

        def asMkBlockLists : List[ List[ MkBlock ] ] =
            list.map( string => List( MkParagraph( string ) ) ).toList
        
        def asMkBlockList : List[ MkBlock ] =
            list.map( string => MkParagraph( string ) ).toList
        
        def hasEmbeddedList : Boolean = list.toList.exists( item => IntermediateParserString(item).hasEmbeddedList )
    }

    private implicit def IntermediateParserStringList( list : Seq[ String ] ) : IntermediateParserStringList = new IntermediateParserStringList( list )
    
    
    private class MkBlockList( list : List[ MkBlock ] ) {
        
        def startsWithMarkdownList : Boolean = {

            if ( list.isEmpty )
                return false

            list.head match {
                case list:MarkdownList => true
                case _ => false
            }
        }
        
        // Don't cry to me if this throws an exception
        def nextToLast : MkBlock = {
            list( list.length - 2 )
        }
    }
    
    private implicit def MkBlockList( list : List[ MkBlock ] ) : MkBlockList = new MkBlockList( list )
    
        
    /**
     * Complex lists are not detected during the initial block parse. This method will locate all
     * lists, and then try to convert the complex block lists into normal lists.
     *
     * There are two main conditions: tight, and sparse.
     *
     * 1. Tight complex lists have items embedded in the list items, as they are not separated by
     *    whitespace lines.
     * 2. Sparse complex lists are broken up by code blocks, where the code block leads with a
     *    bullet or number marker.
     */
    def convertComplexLists( in : List[ MkBlock ], out : List[ MkBlock ] ) : List[ MkBlock ] = {
        
        val tightened = convertSparseComplexLists( in, out )
        val stretched = convertTightComplexLists( tightened, out )
        
        return foldLists( stretched, Nil )
    }
    
    def foldLists( in : List[ MkBlock ], out : List[ MkBlock ] ) : List[ MkBlock ] = {
    
        if ( in.isEmpty ) return out
        
        if ( ! out.isEmpty ) {
            
            in.last match {
            
                case bullet : BulletListMkBlock => {
                
                    out.head match {
                        case complexBullet : ComplexBulletListMkBlock => {
                            return foldLists(
                                in.dropRight( 1 ),
                                ComplexBulletListMkBlock(
                                    bullet.items.asMkBlockLists ::: complexBullet.items
                                )
                                :: out.tail
                            )
                        }
                        case _ => {}
                    }
                }
            
                case complexBullet : ComplexBulletListMkBlock => {
                
                    out.head match {
                        case bullet : BulletListMkBlock => {
                            return foldLists(
                                in.dropRight( 1 ),
                                ComplexBulletListMkBlock(
                                    complexBullet.items ::: bullet.items.asMkBlockLists
                                )
                                :: out.tail
                            )
                        }
                        case _ => {}
                    }
                }
            
                case numbered : NumberedListMkBlock => {
                
                    out.head match {
                        case complexNumbered : ComplexNumberedListMkBlock => {
                            return foldLists(
                                in.dropRight( 1 ),
                                ComplexNumberedListMkBlock(
                                    numbered.items.asMkBlockLists ::: complexNumbered.items
                                )
                                :: out.tail
                            )
                        }
                        case _ => {}
                    }
                }
            
                case complexNumbered : ComplexNumberedListMkBlock => {
                
                    out.head match {
                        case numbered : NumberedListMkBlock => {
                            return foldLists(
                                in.dropRight( 1 ),
                                ComplexNumberedListMkBlock(
                                    complexNumbered.items ::: numbered.items.asMkBlockLists
                                )
                                :: out.tail
                            )
                        }
                        case _ => {}
                    }
                }
                
                case _ => {}
            }
        }
        
        foldLists( in.dropRight( 1 ), in.last :: out )
    }
    
    
    /**
     * Detects the case where a list is trailed by a code block that's really a list. This is done
     * by investigating the iteration when the current node is a code block.
     */
    def convertSparseComplexLists( in : List[ MkBlock ], out : List[ MkBlock ] ) : List[ MkBlock ] = {
        
        if ( in.isEmpty )
            return out

        var convert = false

        if ( in.length > 1 ) {

            in.last match {
                case codeBlock : CodeMkBlock => {
                    convert = (
                        ( in.nextToLast.isInstanceOf[ BulletListMkBlock ] ) ||
                        ( in.nextToLast.isInstanceOf[ NumberedListMkBlock ] )
                    )
                }
            
                case _ => {}
            }
        }
        
        if ( ! convert )
            return convertSparseComplexLists( in.dropRight( 1 ), in.last :: out )
        
        // Add the block to the tail of the preceding list of items.
        
        val codeBlock = in.last.asInstanceOf[ CodeMkBlock ]
        
        in.nextToLast match {
         
            case bulletList   : BulletListMkBlock => {
                
                // The last item is combined with the
                
                val lastItem = MkParagraph( bulletList.items.last ) :: codeBlock.markdown.stripLeadingAndParse
                
                var complex = ComplexBulletListMkBlock(
                    bulletList.items.dropRight( 1 ).asMkBlockLists ::: List( lastItem )
                )
                
                return convertSparseComplexLists( in.dropRight( 2 ), complex :: out )
            }

            case numberedList : NumberedListMkBlock => {
                
                val lastItem = MkParagraph( numberedList.items.last ) :: codeBlock.markdown.stripLeadingAndParse
                
                var complex = ComplexNumberedListMkBlock(
                    numberedList.items.dropRight( 1 ).asMkBlockLists ::: List( lastItem )
                )
                
                return convertSparseComplexLists( in.dropRight( 2 ), complex :: out )
            }
        }
    }
    
    
    /**
     * Converts any lists it finds where items have embedded sequences. An embedded sequence might
     * look like this:
     *
     *     1. First item
     *         * A bullet
     *         * A bullet
     * 
     * Basically, there's no whitespace, so my parser doesn't really know what to do with that.
     *
     * This algorithm goes from the end of the in, and concatenates to the out list. Could be
     * really inefficient, but I don't really care.
     */
    def convertTightComplexLists( in : List[ MkBlock ], out : List[ MkBlock ] ) : List[ MkBlock ] = {
        
        if ( in.isEmpty )
            return out
        
        in.last match {
            
            case numberedList : NumberedListMkBlock => {
                
                if ( numberedList.items.hasEmbeddedList ) {
                    
                    val complexItems = convertNextEmbeddedItem( numberedList.items.reverse, Nil )
                    
                    return convertTightComplexLists(
                        in.dropRight( 1 ),
                        ComplexNumberedListMkBlock( complexItems ) :: out
                    )
                }
            }
            
            case bulletList : BulletListMkBlock => {
                
                if ( bulletList.items.hasEmbeddedList ) {
                    
                    val complexItems = convertNextEmbeddedItem( bulletList.items.reverse, Nil )

                    return convertTightComplexLists(
                        in.dropRight( 1 ),
                        ComplexBulletListMkBlock( complexItems ) :: out
                    )
                }
            }
            
            case _ => {}
        }
        
        convertTightComplexLists( in.dropRight( 1 ), in.last :: out )
    }
    
    private def convertNextEmbeddedItem( in : List[ String ], out : List[ List[ MkBlock ] ] ) : List[ List[ MkBlock ] ] = {
        
        if ( in.isEmpty ) return out
        
        if ( in.head.hasEmbeddedList )
            return convertNextEmbeddedItem( in.tail, in.head.splitToMkBlockList :: out )
        else
            return convertNextEmbeddedItem( in.tail, List( MkParagraph( in.head ) ) :: out )
    }
    

    /**
     * Special rule to group any lists that were separated by whitespace, because it is an option.
     */
    def condenseSpacedLists(
             /** We consume this list and recurse until it's empty. */
             in  : List[ MkBlock ],
             /** Where we stick the output... which, by the way, will be pulled from when condensing. */
             out : Buffer[ MkBlock ]
        ) : List[ MkBlock ] = {

        if ( in.isEmpty )
            return out.toList
            
        var nextBlock = in.head
             
        if ( ! out.isEmpty ) {

            if (
                ( in.head.isInstanceOf[ NumberedListMkBlock ] ) &&
                ( out.last.isInstanceOf[ NumberedListMkBlock ] )
            ) {
                val right = in.head.asInstanceOf[ NumberedListMkBlock ]
                val left  = out.last.asInstanceOf[ NumberedListMkBlock ]

                nextBlock = NumberedListMkBlock( left.items ++ right.items )

                out.trimEnd( 1 )

            } else if (
                ( in.head.isInstanceOf[ BulletListMkBlock ] ) &&
                ( out.last.isInstanceOf[ BulletListMkBlock ] )
            ) {
                val right = in.head.asInstanceOf[ BulletListMkBlock ]
                val left  = out.last.asInstanceOf[ BulletListMkBlock ]
                
                nextBlock = BulletListMkBlock( left.items ++ right.items )

                out.trimEnd( 1 )
            }
         }

         return condenseSpacedLists( in.tail, out + nextBlock )
     }
     
    /**
     * Remove empty MkParagraphs from the beginning and end.
     */
    def trim( list : List[ MkBlock ] ) : List[ MkBlock ] = {

        def nonEmptyMkBlock( b : MkBlock ) : Boolean =
            b.isInstanceOf[MkParagraph] == false || b.asInstanceOf[MkParagraph].markdown.length > 0

        val start = list findIndexOf nonEmptyMkBlock

        val end = list.length - (list.reverse findIndexOf nonEmptyMkBlock)

        list.slice(start, end)
    }
}
