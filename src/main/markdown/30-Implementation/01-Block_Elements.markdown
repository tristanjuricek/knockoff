Block Elements
==============

## `Block` ##

The combination of spanning elements, used by the system. Block elements are
more pronounced in markdown documents than XML documents, as they are usually
delimited by whitespace lines.

Note that some block elements contain other block elements, e.g., lists,
blockquotes. In the case that they do not contain other sub elements, the
first element of the sequence is itself.

    // In knockoff2/Block.scala
    // See the Block package and imports

    trait Block extends BlockSeq {
        
        /**
         * KnockOff's component breakdown of the elements that make up this
         * block.
         */
        val spans    : SpanSeq

        /**
         * A markdown representation of this block. Not necessarilly the
         * original source.
         */
        val markdown : String

        /**
         * An HTML rendering of the Block element.
         */
        val xml      : Elem
        
        /** The original source position used to make up this block. */
        val position : Position
        
        def canEqual( b : Block ) : Boolean
        
    }

In many cases, the Block can not contain other blocks.

    // In knockoff2/SimpleBlock.scala
    package knockoff2
    
    trait SimpleBlock extends Block {
        override def theSeq : Seq[ Block ] = List( this )
    }


## `BlockSeq` ##

The `BlockSeq` adds searching methods over `Block` elements. It is also the
main return type of the `KnockOff.parse` method.

One of the shorthand filter expressions allows for you to indicate the "BlockType".

    // In knockoff2/BlockType.scala
    package knockoff2
    
    /**
     * Used to indicate the type of blocks you are interested in when filtering via
     * the BlockSeq.? method.
     */
    trait BlockType[T <: Block] { def wrappedClass : Class[ T ] }
    case object Paragraphs   extends BlockType[ Paragraph ] { def wrappedClass = classOf[ Paragraph ] }
    case object Headers      extends BlockType[ Header ] { def wrappedClass = classOf[ Header ] }

This is used only as a short hand for query expressions.

    // In knockoff2/BlockSeq.scala
    // See the BlockSeq package and imports
    
    trait BlockSeq extends Seq[ Block ] {
        
        def theSeq : Seq[ Block ]
        
        override def length : Int = theSeq.length
        
        override def elements = theSeq.elements
        
        override def apply( ii : Int ) = theSeq(ii)

        /**
         * Returns a BlockSeq that contains only that particular block type.
         */
        def ? [ T <: Block ] ( blockType : BlockType[T] ) : BlockSeq = {
            BlockSeq.fromSeq( filter( blockType.wrappedClass.isInstance( _ ) ) )
        }
        
        
        /** Shorthand for the filter method. */
        def ? ( query : Block => Boolean ) : BlockSeq =
            BlockSeq.fromSeq( filter( query ) )
    }

    object BlockSeq {
        
        def fromSeq( seq : Seq[ Block ] ) = new BlockSeq {
            override def theSeq = seq
        }
    }


## `Paragraph` ##

The text block element that is represented inside a `<p>` tag. Does not
contain any other useful metadata.

Otherwise the paragraph is a simple `Block` type (does not contain other
`Block`s).

    // In knockoff2/Paragraph.scala
    // See the Paragraph package and imports

    class Paragraph(
        val spans       : SpanSeq,
        val markdown    : String,
        val xml         : Elem,
        val position    : Position
    )
    extends SimpleBlock {
        
        def canEqual( b : Block ) = b.isInstanceOf[ Paragraph ]
        
        override def toString = "Paragraph(" + spans + ")"
    }


## `Header` ##

Represents an `<h1>`, `<h2>`, etc., element in the markdown document.

The header element is a simple `Block` type, but can contain any kind of
`Span`.

The `markdown` representation of the Header should always be the ATX-style
headers - `# Header #`.

    // In knockoff2/Header.scala
    // See the Header package and imports
    
    class Header(
        val level       : Int,
        val spans       : SpanSeq,
        val markdown    : String,
        val xml         : Elem,
        val position    : Position
    )
    extends SimpleBlock {

        def canEqual( b : Block ) = b.isInstanceOf[ Header ]
        
        override def toString = "Header(" + level + ", " + spans + ")"
    }


## Basic Verification ##

    // In test knockoff2/BlockSuite.scala
    package knockoff2

    import org.scalatest._
    import matchers._

    class BlockSuite extends Spec with ShouldMatchers with ElementFactory {
        describe("BlockSeq") {
            it( "should filter Paragraphs and Headers properly" ) {

                val p1 = Paragraph( "p1", Position(0,0) )
                val h1 = Header( 1, "h1", Position(0,0) )

                val blocks = BlockSeq.fromSeq( p1 :: h1 :: Nil )
                
                ( blocks ? Paragraphs ) should have length (1)
                assert( ( blocks ? Paragraphs ) contains p1 )
                                
                ( blocks ? Headers ) should have length (1)
                assert( ( blocks ? Headers ) contains h1 )
            }
        }
    }


## References ##

#### Block - Package And Imports

    // The Block package and imports
    package knockoff2
    
    import scala.xml.Elem

#### BlockSeq - Package And Imports

    // The BlockSeq package and imports
    package knockoff2

    import scala.xml.Elem

#### Paragraph - Package And Imports

    // The Paragraph package and imports
    package knockoff2

    import scala.xml.Elem

#### Header - Package And Imports

    // The Header package and imports
    package knockoff2

    import scala.xml.Elem
