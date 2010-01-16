Block Elements
==============

## `Block` ##

`Block`s are kind of the grouping elements of a system. Other things are laid out
inside of `Blocks`: namely `Span`s, but also, other blocks. In the markdown, blocks
are usually seen as separated by whitespace lines.

    // In com/tristanhunt/knockoff/Block.scala
    // See the Block package and imports

    trait Block extends BlockSeq {
      
      /**
        The actual content of each block.
       */
      val span     : SpanSeq
      
      /**
        A markdown representation of this block - it may not equal the original
        source.
       */
      def markdown : String
      
      /**
        An HTML rendering of the Block element.
       */
      def xml      : Node
      
      /**
        The original source position used to make up this block.
       */
      val position : Position
    }

In many cases, the Block can not contain other blocks.

    // In com/tristanhunt/knockoff/SimpleBlock.scala
    package com.tristanhunt.knockoff
    
    trait SimpleBlock extends Block {
      
        override def theSeq : Seq[ Block ] = List( this )
    }

In some other cases, the block is a pretty complex thing:

    // In com/tristanhunt/knockoff/ComplexBlock.scala
    package com.tristanhunt.knockoff
    
    trait ComplexBlock extends Block {

        val children : Seq[ Block ]
        
        val span = new SpanSeq{ def theSeq = children.flatMap( _.span ) }
        
        def theSeq = children
        
        def childrenMarkdown = children.map( _.markdown ).mkString("\n") + "\n"
        
        def childrenXML = children.map( _.xml )
    }

There are moments when we need to capture a series of blocks as a `ComplexBlock`
without any other real information.

    // In com/tristanhunt/knockoff/GroupBlock.scala
    package com.tristanhunt.knockoff
    
    import scala.xml.{ Node, Group }
    import scala.util.parsing.input.{ NoPosition, Position }
    
    class   GroupBlock( val children : Seq[ Block ] )
    extends ComplexBlock {
     
      val position : Position = children.firstOption match {
        case None => NoPosition
        case Some( child ) => child.position
      }
         
      def markdown = childrenMarkdown
      
      def xml : Node = Group( children.map( _.xml ) )
  
      override def toString = "GroupBlock(" + markdown + ")"
    }


## `BlockSeq` ##

The `BlockSeq` adds searching methods over `Block` elements. It is also the
main return type of the `KnockOff.parse` method.

One of the shorthand filter expressions allows for you to indicate the "BlockType".

    // In com/tristanhunt/knockoff/BlockType.scala
    package com.tristanhunt.knockoff
    
    /**
     * Used to indicate the type of blocks you are interested in when filtering via
     * the BlockSeq.filterType method.
     */
    trait BlockType[T <: Block] { def wrappedClass : Class[ T ] }
    
    case object Paragraphs
    extends BlockType[ Paragraph ] { def wrappedClass = classOf[ Paragraph ] }
    
    case object Headers
    extends BlockType[ Header ] { def wrappedClass = classOf[ Header ] }
    
    case object LinkDefinitions
    extends BlockType[ LinkDefinition ] {
      def wrappedClass = classOf[ LinkDefinition ]
    }
    
    case object Blockquotes
    extends BlockType[ Blockquote ] { def wrappedClass = classOf[ Blockquote ] }
    
    case object CodeBlocks
    extends BlockType[ CodeBlock ] { def wrappedClass = classOf[ CodeBlock ] }
    
    case object HorizontalRules
    extends BlockType[ HorizontalRule ] {
      def wrappedClass = classOf[ HorizontalRule ]
    }
    
    case object OrderedItems
    extends BlockType[ OrderedItem ] { def wrappedClass = classOf[ OrderedItem ] }
    
    case object UnorderedItems
    extends BlockType[ UnorderedItem ] {
      def wrappedClass = classOf[ UnorderedItem ]
    }
    
    case object MarkdownLists
    extends BlockType[ MarkdownList ] { def wrappedClass = classOf[ MarkdownList ] }

This is used only as a short hand for query expressions.

    // In com/tristanhunt/knockoff/BlockSeq.scala
    // See the BlockSeq package and imports
    
    trait BlockSeq extends Seq[ Block ] {
      
      def theSeq : Seq[ Block ]
      
      override def length : Int = theSeq.length
      
      override def elements = theSeq.elements
      
      override def apply( ii : Int ) = theSeq(ii)

      def toXML : Node = Group( theSeq.map( _.xml ) )

      /**
       * Returns a BlockSeq that contains only that particular block type.
       */
      def filterType [ T <: Block ] ( blockType : BlockType[T] ) : BlockSeq = {
        BlockSeq.fromSeq( filter( blockType.wrappedClass.isInstance( _ ) ) )
      }
      
      
      /** Shorthand for the filter method. */
      def filterType ( query : Block => Boolean ) : BlockSeq =
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

    // In com/tristanhunt/knockoff/Paragraph.scala
    // See the Paragraph package and imports

    class   Paragraph( val span : SpanSeq, val position  : Position )
    extends SimpleBlock {

      def markdown = span.toMarkdown

      /**
        If this paragraph only contains HTMLSpan elements, then just pass that
        information through without a paragraph marker.
      */
      def xml =
        if ( isHTML ) span.toXML else <p>{ span.toXML }</p>
      
      def isHTML : Boolean = ! span.exists(
        s => s match {
          case html : HTMLSpan => false
          case text : Text     => ! text.content.trim.isEmpty
          case _               => true
        }
      )
      
      // See the Paragraph toString, equals, hashCode implementations
    }


## `Header` ##

Represents an `<h1>`, `<h2>`, etc., element in the markdown document.

The header element is a simple `Block` type, but can contain any kind of
`SpanSeq`.

The `markdown` representation of the Header should always be the ATX-style
headers - `# Header #`.

    // In com/tristanhunt/knockoff/Header.scala
    // See the Header package and imports
    
    class   Header( val level : Int, val span : SpanSeq, val position : Position )
    extends SimpleBlock {

      def markdown = {
        val sb = new StringBuilder
        ( 0 until level ).foreach( _ => sb.append("#") )
        sb.append(" ").append( span.toMarkdown ).append(" ")
        ( 0 until level ).foreach( _ => sb.append("#") )
        sb.toString
      }
      
      def xml = level match {
        case 1 => <h1>{ span.toXML }</h1>
        case 2 => <h2>{ span.toXML }</h2>
        case 3 => <h3>{ span.toXML }</h3>
        case 4 => <h4>{ span.toXML }</h4>
        case 5 => <h5>{ span.toXML }</h5>
        case 6 => <h6>{ span.toXML }</h6>
        case _ => <div class={ "header" + level }>{ span.toXML }</div>
      }

      // See the Header toString, equals, hashCode implementations
    }


## `LinkDefinition` ##

When a link is created with the definition like `[something][id]`, that `id` can
be defined later on a string like `[id]: url "optional title"`.

    // In com/tristanhunt/knockoff/LinkDefinition.scala
    // See the LinkDefinition package and imports
    
    class LinkDefinition(
      val id        : String,
      val url       : String,
      val title     : Option[ String ],
      val position  : Position
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

    // In com/tristanhunt/knockoff/Blockquote.scala
    // See the Blockquote package and imports

    class Blockquote(
      val children : Seq[ Block ],
      val position : Position
    )
    extends ComplexBlock {
                
      def markdown : String = {
        Source.fromString( childrenMarkdown )
          .getLines.map( l => "> " + l ).mkString("")
      }
      
      def xml : Elem = <blockquote>{ childrenXML }</blockquote>
      
      // See the Blockquote toString, equals, hashCode implementations
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

    // In com/tristanhunt/knockoff/CodeBlock.scala
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
          
      def xml : Node = <pre><code>{ preformatted }</code></pre>
      
      // See the CodeBlock toString, equals, hashCode implementations
    }


## `HorizontalRule` ##

Represents a `<hr/>` injected into content. Note that this does not happen to do
anything but replace a line of asterixes, underscores, or hyphens.

    // In com/tristanhunt/knockoff/HorizontalRule.scala
    package com.tristanhunt.knockoff
    // See the HorizontalRule imports
    
    class HorizontalRule( val position : Position ) extends SimpleBlock {
      
      def markdown = "* * *"
      
      val span = new Text( markdown )
      
      def xml = <hr/>

      // See the HorizontalRule toString, equals, hashCode implementations
    }

## Lists - Unordered, Ordered, Simple, and Complex ##

Lists in markdown can be simple or complex. It becomes complex when any one of the
list items have more than one `Block`. They are either tagged as ordered or
unordered, though the `ElementFactory` has helper methods to make this decision.

How this might look in code:

    val olist = olist(
      oi( para("some text") ),
      oi( para("uno"), para("dos") )
    )
    
    val ulist = ulist(
      ui("list item 1"),
      ui("list item 2")
    )

Note the difference between the list items `oi` of the `olist` and the `ui` of the
`ulist`. This makes discovery of the list much simpler during the parsing process,
and prevents you from mistakenly globbing inconsistent lists together.

These codes would be represented as:

    <ol>
      <li><p>some text</p></li>
      <li><p>uno</p><p>dos</p><li>
    </ol>
    
    <ul>
      <li>list item 1</li>
      <li>list item 2</li>
    </ul>

Only in the simple list case do we let the `<li>` operate as a simple implicit block
container.

In implementation terms, we don't have a single list.

#### `ListItem`

    // In com/tristanhunt/knockoff/ListItem.scala
    package com.tristanhunt.knockoff
    // See the ListItem imports
    
    abstract class ListItem(
      val items    : Seq[ Block ],
      val position : Position
    )
    extends ComplexBlock {
      
      val children = new GroupBlock( items )

      def itemPrefix : String
      
      def markdown : String = {
        if ( children.isEmpty ) return ""

        return (
          ( itemPrefix + children.first.markdown + "  \n" ) +
          children.drop(1).map( "    " + _.markdown + "  \n" ).mkString( "" )
        )
      }
      
      def xml( complex : Boolean ) : Node = <li>{
        if ( isComplex )
          childrenXML 
        else
          children.first.span.toXML
      }</li>
      
      def xml : Node = xml( isComplex )
      
      def isComplex = items.length > 1
      
      def + ( block : Block ) : ListItem
      
      // See the ListItem toString, equals, hashCode implementation
    }
      
### `OrderedItem`

    // In com/tristanhunt/knockoff/OrderedItem.scala
    package com.tristanhunt.knockoff

    import scala.util.parsing.input.Position
    
    class OrderedItem( items : Seq[ Block ], position : Position )
    extends ListItem( items, position ) {

      def this( block : Block, position : Position ) =
        this( Seq( block ), position )
      
      def itemPrefix = "1. "
      
      def + ( b : Block ) : ListItem =
        new OrderedItem( children ++ Seq(b), children.first.position )
    }

### `UnorderedItem`

    // In com/tristanhunt/knockoff/UnorderedItem.scala
    package com.tristanhunt.knockoff
    
    import scala.util.parsing.input.Position
        
    class UnorderedItem( items : Seq[ Block ], position : Position )
    extends ListItem( items, position ) {
      
      def this( block : Block, position : Position ) =
        this( Seq( block ), position )
      
      def itemPrefix = "* "
      
      def + ( b : Block ) : ListItem =
        new UnorderedItem( children ++ Seq(b), children.first.position )
    }

#### `MarkdownList`

Lists just contain items, and are either ordered or unordered.

The position of a list is a little spurious: the start of the list should be
the position of the first item, however, it's elements may not contain the
entire content; whitespace will be missing in complex cases.

    // In com/tristanhunt/knockoff/MarkdownList.scala
    package com.tristanhunt.knockoff
    // See the MarkdownList imports
    
    /**
     * @param ordered Alters the output, mostly.
     */
    abstract class MarkdownList(
      val items : Seq[ ListItem ]
    ) extends ComplexBlock {
      
      val children = items
      
      val position = children.firstOption match {
        case None => NoPosition
        case Some( child ) => child.position
      }
      
      def markdown = childrenMarkdown
      
      /**
        Create a new list with the block appended to the last item.
      */
      def + ( block : Block ) : MarkdownList 
      
      // See the MarkdownList toString, equals, hashCode implementations
    }
    
    class OrderedList( items : Seq[ ListItem ] )
    extends MarkdownList( items ) {
     
      def xml = <ol>{ childrenXML }</ol>
      
      def + ( item : OrderedItem ) : OrderedList =
        new OrderedList( children ++ Seq(item) )
    
      def + ( block : Block ) : MarkdownList = new OrderedList(
        children.take( children.length - 1 ) ++ Seq(children.last + block)
      )
    }
    
    class UnorderedList( items : Seq[ ListItem ] )
    extends MarkdownList( items ) {
     
      def xml = <ul>{ childrenXML }</ul>
      
      def + ( item : UnorderedItem ) : UnorderedList =
        new UnorderedList( children ++ Seq(item) )

      def + ( block : Block ) : MarkdownList = new UnorderedList(
        children.take( children.length - 1 ) ++ Seq(children.last + block)
      )
    }
    

## Block Specification ##

    // In test com/tristanhunt/knockoff/BlockSuite.scala
    package com.tristanhunt.knockoff

    import scala.util.parsing.input.NoPosition
    import org.scalatest._
    import matchers._

    class BlockSuite extends Spec with ShouldMatchers with HasElementFactory {

      val factory = elementFactory
      import factory._

      describe("BlockSeq") {

        it( "should filter Paragraphs and Headers properly with filterType" ) {

          val p1 = para( t("p1"), NoPosition )
          val h1 = head( 1, t("h1"), NoPosition )

          val blocks = BlockSeq.fromSeq( List( p1, h1 ) )
          
          ( blocks filterType Paragraphs ) should have length (1)
          assert( ( blocks filterType Paragraphs ) contains p1 )
          
          ( blocks filterType Headers ) should have length (1)
          assert( ( blocks filterType Headers ) contains h1 )
        }
      }
    }


## References ##

### `Block`

    // The Block package and imports
    package com.tristanhunt.knockoff
    
    import scala.xml.Node
    import scala.util.parsing.input.Position

### `BlockSeq`

    // The BlockSeq package and imports
    package com.tristanhunt.knockoff

    import scala.xml.{ Node, Elem, Group }


### `Paragraph`

    // The Paragraph package and imports
    package com.tristanhunt.knockoff

    import scala.xml.Elem
    import scala.util.parsing.input.Position

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

    // The Header package and imports
    package com.tristanhunt.knockoff

    import scala.xml.Elem
    import scala.util.parsing.input.Position

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

    // The LinkDefinition package and imports
    package com.tristanhunt.knockoff

    import scala.xml.{ Node, Group }
    import scala.util.parsing.input.Position

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

    // The Blockquote package and imports
    package com.tristanhunt.knockoff
    
    import scala.io.Source
    import scala.xml.Elem
    import scala.util.parsing.input.Position

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
      43 + (
        ( 3 /: children ){
          (sum, child) => { sum + 43 + 3 * child.hashCode }
        }
      )
    }


### `CodeBlock`

    // The CodeBlock package and imports
    package com.tristanhunt.knockoff

    import scala.xml.{ Node, Unparsed }
    import scala.io.Source
    import scala.util.parsing.input.Position

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


### `HorizontalRule`

    // The HorizontalRule imports

    import scala.xml.{ Node, Unparsed }
    import scala.io.Source
    import scala.util.parsing.input.Position

    // The HorizontalRule toString, equals, hashCode implementations
    override def toString = "HorizontalRule"

    override def hashCode : Int = position.hashCode + 47

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : HorizontalRule => t.canEqual( this ) && ( t.position == position )
      case _ => false
    }

    def canEqual( t : HorizontalRule ) : Boolean = t.getClass == getClass


### `ListItem`

    // The ListItem imports
    
    import scala.util.parsing.input.Position
    import scala.xml.Node

    // The ListItem toString, equals, hashCode implementations
    override def toString = "ListItem(" + markdown + ")"

    override def hashCode : Int =  {
      ( 11 /: children )( (total, child) => total + 51 + 3 * child.hashCode ) +
      position.hashCode + 47
    }

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : ListItem => t.canEqual( this ) && ( this sameElements t )
      case _ => false
    }
    
    def sameElements( ci : ListItem ) : Boolean = {
      ( children == ci.children ) &&
      ( position == ci.position )
    }

    def canEqual( t : ListItem ) : Boolean = t.getClass == getClass



### `MarkdownList`

    // The MarkdownList imports
    
    import scala.io.Source
    import scala.util.parsing.input.{ NoPosition, Position }
    
    // The MarkdownList toString, equals, hashCode implementations
    override def toString = "MarkdownList(" + markdown + ")"

    override def hashCode : Int = {
      ( 13 /: children )( (total, child) => total + 51 + 3 * child.hashCode ) +
      position.hashCode + 47
    }

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : MarkdownList => t.canEqual( this ) && ( t sameElements this )
      case _ => false
    }
    
    def sameElements( ml : MarkdownList ) : Boolean = {
      ( children sameElements ml.children )
    }

    def canEqual( t : MarkdownList ) : Boolean = t.getClass == getClass
