Block Elements
==============

## `Block` ##

`Block`s are kind of the grouping elements of a system. Other things are laid out
inside of `Blocks`: namely `Span`s, but also, other blocks. In the markdown, blocks
are usually seen as separated by whitespace lines.

    // In knockoff2/Block.scala
    // See the Block package and imports

    trait Block extends BlockSeq {
        
        /**
         * The actual content of each block.
         */
        val span     : Span

        /**
         * A markdown representation of this block. Not necessarilly the
         * original source.
         */
        def markdown : String

        /**
         * An HTML rendering of the Block element.
         */
        def xml      : Node
        
        /** The original source position used to make up this block. */
        val position : Position
    }

In many cases, the Block can not contain other blocks.

    // In knockoff2/SimpleBlock.scala
    package knockoff2
    
    trait SimpleBlock extends Block {
        override def theSeq : Seq[ Block ] = List( this )
    }

In some other cases, the block is a pretty complex thing:

    // In knockoff2/ComplexBlock.scala
    package knockoff2
    
    trait ComplexBlock extends Block {
        val children : BlockSeq
        def theSeq = children
        def childrenMarkdown = children.map( _.markdown ).mkString("\n")
        def childrenXML = children.map( _.xml )
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
    
    case object Paragraphs
    extends BlockType[ Paragraph ] { def wrappedClass = classOf[ Paragraph ] }
    
    case object Headers
    extends BlockType[ Header ] { def wrappedClass = classOf[ Header ] }

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


## `Position` ##

A somewhat special utility that allows each `Block` to know where it comes from.

    // In knockoff2/Position.scala
    package knockoff2

    import scala.io.Source

    case class Position (
        val linesStart  : Int,
        val linesEnd    : Int,
        val origin      : Source
    )

## `Paragraph` ##

The text block element that is represented inside a `<p>` tag. Does not
contain any other useful metadata.

Otherwise the paragraph is a simple `Block` type (does not contain other
`Block`s).

    // In knockoff2/Paragraph.scala
    // See the Paragraph package and imports

    class Paragraph(
        val span        : Span,
        val position    : Position
    )
    extends SimpleBlock {

        def markdown = span.markdown

        def xml = <p>{ span.xml }</p>
        
        // See the Paragraph toString, equals, hashCode implementations
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
        val span        : Span,
        val position    : Position
    )
    extends SimpleBlock {

        def markdown = {
            val sb = new StringBuilder
            ( 0 until level ).foreach( _ => sb.append("#") )
            sb.append(" ").append( span.markdown ).append(" ")
            ( 0 until level ).foreach( _ => sb.append("#") )
            sb.toString
        }
        
        def xml = level match {
            case 1 => <h1>{ span.xml }</h1>
            case 2 => <h2>{ span.xml }</h2>
            case 3 => <h3>{ span.xml }</h3>
            case 4 => <h4>{ span.xml }</h4>
            case 5 => <h5>{ span.xml }</h5>
            case 6 => <h6>{ span.xml }</h6>
            case _ => <div class={ "header" + level }>{ span.xml }</div>
        }

        // See the Header toString, equals, hashCode implementations
    }


## `LinkDefinition` ##

When a link is created with the definition like `[something][id]`, that `id` can
be defined later on a string like `[id]: url "optional title"`.

    // In knockoff2/LinkDefinition.scala
    // See the LinkDefinition package and imports
    
    class LinkDefinition(
        val id          : String,
        val url         : String,
        val title       : Option[ String ],
        val position    : Position
    )
    extends SimpleBlock {

        val span = Span.empty
     
        def xml : Node = Group( Nil )
        
        def markdown = {
            val sb = new StringBuilder
            sb.append("[").append( id ).append("]: ").append( url ).append(
                title match {
                    case None => ""
                    // Remember that "This "Title" is Valid" as a single title.
                    case Some( titleValue ) => "\"" + titleValue + "\""
                }
            )
            sb.toString
        }
        
        // See the LinkDefinition toString, equals, hashCode implementations
    }


## `Blockquote` ##

A block quote is really another markdown document, quoted.

    // In knockoff2/Blockquote.scala
    // See the Blockquote package and imports

    class Blockquote(
        val children : BlockSeq,
        val position : Position
    )
    extends ComplexBlock {
        
        val span = new GroupSpan( children.map( _.span ) )
        
        def markdown : String = {
            Source.fromString( childrenMarkdown ).getLines.map { line =>
                "> " + line
            }.mkString( "" )
        }
        
        def xml : Elem = <blockquote>{ childrenXML }</blockquote>
        
        // See the Blockquote toString, equals, hashCode implementations
    }


## Block Specification ##

    // In test knockoff2/BlockSuite.scala
    package knockoff2

    import org.scalatest._
    import matchers._

    class BlockSuite extends Spec with ShouldMatchers with ElementFactory {
        describe("BlockSeq") {

            it( "should filter Paragraphs and Headers properly with ?" ) {

                val p1 = para( t("p1"), emptyPos )
                val h1 = head( 1, t("h1"), emptyPos )

                val blocks = BlockSeq.fromSeq( List( p1, h1 ) )
                
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
    
    import scala.xml.Node

#### BlockSeq - Package And Imports

    // The BlockSeq package and imports
    package knockoff2

    import scala.xml.Elem

### `Paragraph`

#### `Paragraph` - Package And Imports

    // The Paragraph package and imports
    package knockoff2

    import scala.xml.Elem

#### `Paragraph` - `toString`, `equals`, `hashCode`

    // The Paragraph toString, equals, hashCode implementations
    override def toString = "Paragraph(" + markdown + ")"
    
    override def equals( rhs : Any ):Boolean = rhs match {
        case oth : Paragraph => ( oth canEqual this ) && ( this sameElements oth )
        case _ => false
    }
    
    def canEqual( p : Paragraph ) : Boolean = ( getClass == p.getClass )
    
    def sameElements( p : Paragraph ) : Boolean = {
        ( span == p.span ) &&
        ( position == p.position )
    }
    
    override def hashCode : Int = span.hashCode + position.hashCode

### `Header`

#### `Header` - Package And Imports

    // The Header package and imports
    package knockoff2

    import scala.xml.Elem

#### `Header` - `toString`, `equals`, `hashCode`

    // The Header toString, equals, hashCode implementations
    override def toString = "Header(" + markdown + ")"

    override def equals( rhs : Any ):Boolean = rhs match {
        case oth : Header => ( oth canEqual this ) && ( this sameElements oth )
        case _ => false
    }

    def canEqual( p : Header ) : Boolean = ( getClass == p.getClass )

    def sameElements( p : Header ) : Boolean = {
        ( level == p.level ) &&
        ( span == p.span ) &&
        ( position == p.position )
    }

    override def hashCode : Int =
        43 + level + span.hashCode + position.hashCode


### `LinkDefinition`

#### `LinkDefinition` - Package And Imports

    // The LinkDefinition package and imports
    package knockoff2

    import scala.xml.{ Node, Group }

#### `LinkDefinition` - `toString`, `equals`, `hashCode`

    // The LinkDefinition toString, equals, hashCode implementations
    override def toString = "LinkDefinition(" + markdown + ")"

    override def equals( rhs : Any ):Boolean = rhs match {
        case oth : LinkDefinition => ( oth canEqual this ) && ( this sameElements oth )
        case _ => false
    }

    def canEqual( p : LinkDefinition ) : Boolean = ( getClass == p.getClass )

    def sameElements( p : LinkDefinition ) : Boolean = {
        ( id == p.id ) &&
        ( url == p.url ) &&
        ( title == p.title ) &&
        ( position == p.position )
    }

    override def hashCode : Int =
        43 + id.hashCode + url.hashCode + span.hashCode + position.hashCode

### `BlockQuote`

#### `BlockQuote` - Package And Imports

    // The Blockquote package and imports
    package knockoff2
    
    import scala.io.Source
    import scala.xml.Elem

#### `Blockquote` - `toString`, `equals`, `hashCode`

    // The Blockquote toString, equals, hashCode implementations
    override def toString = "Blockquote(" + markdown + ")"
    
    override def equals( rhs : Any ):Boolean = rhs match {
        case oth : Blockquote => ( oth canEqual this ) && ( this sameElements oth )
        case _ => false
    }
    
    def canEqual( b : Blockquote ) : Boolean = ( getClass == b.getClass )
    
    def sameElements( b : Blockquote ) = {
        ( children sameElements b.children ) &&
        ( position == b.position )
    }
    
    override def hashCode : Int = {
        43 + ( ( 3 /: children )( (sum, child) => {
            sum + 43 + 3 * child.hashCode
        } ) )
    }