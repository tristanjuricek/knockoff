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
         * A markdown representation of this block - it may not equal the original
         * source.
         */
        def markdown : String

        /**
         * An HTML rendering of the Block element.
         */
        def xml      : Node
        
        /**
         * The original source position used to make up this block.
         */
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


## `HTMLBlock` ##

We consider this to be already formatted HTML. The content here is specificed as a
string - everything else is just basically passed directly back.

    // In knockoff2/HTMLBlock.scala
    // See the HTMLBlock package and imports
    
    class   HTMLBlock( val html : String, val position : Position )
    extends SimpleBlock {
        
        val span = new HTMLSpan( html )
        
        def xml : Node = Unparsed( html )
        
        def markdown = html
        
        // See the HTMLBlock toString, equals, hashCode implementations
    }

## `CodeBlock` ##

The code block is a chunk of preformatted text to this system. Note that this means
we completely ignore all formatting, and do escaping. This means that sequences like

    <later>

become this in the output:

    <pre><code>&le;later&gt;</pre></code>

This means that in order to inject actual HTML inside the final code, you'll have to
write up an HTML code element. This could be seen as a later transformation, say,
if you want to inject a series of line numbers via `<span>` elements.

    // In knockoff2/CodeBlock.scala
    // See the CodeBlock package and imports
    
    class   CodeBlock( val text : Text, val position : Position )
    extends SimpleBlock {

        def this( preformatted : String, position : Position ) =
            this( new Text( preformatted ), position )

        val span = text
     
        val preformatted = text.markdown
        
        lazy val preformattedLines =
            Source.fromString( preformatted ).getLines
        
        def markdown =
            preformattedLines.map{ line =>  "    " + line }.mkString("")
            
        def xml : Node = <pre><code>{ Unparsed( preformatted ) }</code></pre>
        
        // See the CodeBlock toString, equals, hashCode implementations
    }


## `HorizontalRule` ##

Represents a `<hr/>` injected into content. Note that this does not happen to do
anything but replace a line of asterixes, underscores, or hyphens.

    // In knockoff2/HorizontalRule.scala
    package knockoff2
    
    class HorizontalRule( val position : Position ) extends SimpleBlock {
        
        def markdown = "* * *"
        
        val span = new Text( markdown )
        
        def xml = <hr/>

        // See the HorizontalRule toString, equals, hashCode implementations
    }


## TODO :  List


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

### `Blockquote`

#### `Blockquote` - Package And Imports

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

### `HTMLBlock`

#### `HTMLBlock` - Package and Imports

    // The HTMLBlock package and imports
    package knockoff2

    import scala.xml.{ Node, Unparsed }

#### `HTMLBlock` - `toString`, `equals`, `hashCode`

    // The HTMLBlock toString, equals, hashCode implementations
    override def toString = "HTMLBlock(" + html + ")"
    
    override def hashCode : Int = html.hashCode
        
    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : HTMLBlock => t.canEqual( this ) && ( this sameElements t )
        case _ => false
    }
    
    def sameElements( h : HTMLBlock ) : Boolean = {
        ( h.html == html ) &&
        ( h.position == position )
    }
    
    def canEqual( t : HTMLBlock ) : Boolean = t.getClass == getClass

### `CodeBlock`

#### `CodeBlock` - Package and Imports

    // The CodeBlock package and imports
    package knockoff2

    import scala.xml.{ Node, Unparsed }
    import scala.io.Source

#### `CodeBlock` - `toString`, `equals`, `hashCode`

    // The CodeBlock toString, equals, hashCode implementations
    override def toString = "CodeBlock(" + preformatted + ")"

    override def hashCode : Int = preformatted.hashCode

    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : CodeBlock => t.canEqual( this ) && ( this sameElements t )
        case _ => false
    }
    
    def sameElements( cb : CodeBlock ) : Boolean = {
        ( cb.preformatted == preformatted ) &&
        ( cb.position == position )
    }

    def canEqual( t : CodeBlock ) : Boolean = t.getClass == getClass

#
### `HorizontalRule`

#### `HorizontalRule` - Package and Imports

    // The HorizontalRule package and imports
    package knockoff2

    import scala.xml.{ Node, Unparsed }
    import scala.io.Source

#### `HorizontalRule` - `toString`, `equals`, `hashCode`

    // The HorizontalRule toString, equals, hashCode implementations
    override def toString = "HorizontalRule"

    override def hashCode : Int = position.hashCode + 47

    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : HorizontalRule => t.canEqual( this ) && ( t.position == position )
        case _ => false
    }

    def canEqual( t : HorizontalRule ) : Boolean = t.getClass == getClass