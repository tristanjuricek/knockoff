Spanning Elements
=================

## `Span` ##

Marks a string to become the basic building block of the Markdown tree.

Each Span is also a sequence of other Spans - some elements, like a link definition,
can have part of it's description be code.

    // In knockoff/Span.scala
    package knockoff
    
    import scala.xml.Node
    
    trait Span extends SpanSeq {
      def markdown : String
      def xml : Node
    }
    
    object Span {
      val empty : Span = new Text("")
    }

## `SpanSeq` ##

Each `Block` is composed of these.

    // In knockoff/SpanSeq.scala
    package knockoff
    
    import scala.xml.Group
    
    trait SpanSeq extends Seq[ Span ] {
      def theSeq : Seq[ Span ]
      override def length : Int = theSeq.length
      override def elements = theSeq.elements
      override def apply( ii : Int ) = theSeq(ii)
      
      def toXML = Group( theSeq.flatMap( _.xml ) )
      
      def toMarkdown = theSeq.map( _.markdown ).mkString("")
    }

A simpler version is for the common case, where the span does not actually contain
other spans.

    // In knockoff/SimpleSpan.scala
    package knockoff
    
    trait SimpleSpan extends Span {
      def theSeq = List( this )
    }

The other, complex case is where a span contains a straight list of children.

    // In knockoff/ComplexSpan.scala
    package knockoff
    
    trait ComplexSpan extends Span {
      val children : Seq[ Span ]
      def theSeq = children
      def childrenMarkdown = children.map( _.markdown ).mkString("")
      def childrenXML = toXML
    }

And a workaround to cases where we need just a container of spans.

    // In knockoff/GroupSpan.scala
    package knockoff
    
    import scala.xml.Group
    
    class GroupSpan( val children : SpanSeq ) extends ComplexSpan {
     
      def this( seq : Seq[ Span ] ) {
        this( new SpanSeq { def theSeq = seq } )
      }
      
      def xml = toXML
      
      def markdown = toMarkdown
    }



## `Text` ##

The most basic Span element that contains no other markup information.

    // In knockoff/Text.scala
    package knockoff
    
    import scala.xml.{ Node, Text => XMLText }
    
    class Text( val content : String ) extends SimpleSpan {

      def markdown = content

      def xml : Node =
        XMLText( unescape( content ) )
      
      val escapeableChars = List(
          "\\", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!", ">"
      )

      def unescape(source:String):String = {
          var buf:String = source
          for ((escaped, unescaped) <- escapeableChars.map(ch => ("\\" + ch, ch)))
              buf = buf.replace(escaped, unescaped)
          buf
      }

      // See the Text toString, hashCode, equals implementations
    }

## `HTMLSpan` ##

These sequences are found inside of blocks, but still mean "just pass it on".

    // In knockoff/HTMLSpan.scala
    package knockoff
    
    import scala.xml.{ Node, Unparsed }
    
    class HTMLSpan( val content : String ) extends SimpleSpan {

      def markdown = content

      def xml : Node = Unparsed( content )

      // See the HTMLSpan toString, hashCode, equals implementations       
    }

## `CodeSpan` ##

These are usually represented by inline `<code>` blocks in paragraph text. This is
not to be confused with `CodeBlock` - a `CodeBlock` does not contain a `CodeSpan`.

    // In knockoff/CodeSpan.scala
    package knockoff
    
    import scala.xml.Node
    
    class CodeSpan( val content : String ) extends SimpleSpan {

      def markdown = content

      def xml : Node = <code>{ content }</code>

      // See the CodeSpan toString, hashCode, equals implementations       
    }

## `Strong` ##

These emphasize other spans, usually with `<strong>` tags.

    // In knockoff/Strong.scala
    package knockoff
    
    import scala.xml.Node

    class Strong( val children : Seq[ Span ] ) extends ComplexSpan {
      
      def markdown = "**" + childrenMarkdown + "**"
      
      def xml : Node = <strong>{ childrenXML }</strong>
      
      // See the Strong toString, hashCode, equals implementations
    }

## `Emphasis` ##

Wraps other spans with `<em>` tags.

    // In knockoff/Emphasis.scala
    package knockoff

    import scala.xml.Node

    class Emphasis( val children : Seq[ Span ] ) extends ComplexSpan {

      def markdown = "_" + childrenMarkdown + "_"

      def xml : Node = <em>{ childrenXML }</em>

      // See the Emphasis toString, hashCode, equals implementations
    }

## Links ##

Links are kind of special spanning elements, because there are

1. Direct links, e.g., `[title](url)`
2. Indirect links, e.g., `[title][reference]`
3. Image links, e.g., `![title](url)`

### `Link`

The direct link is is simply called a `Link`.

    // In knockoff/Link.scala
    package knockoff

    import scala.xml.Node

    class Link(
      val children  : SpanSeq,
      val url       : String,
      val title     : Option[ String ]
    )
    extends ComplexSpan {
        
      def markdown = {
        "[" + childrenMarkdown + "](" +
        url + {
          title match {
            case Some( titleString ) => " \"" + titleString + "\""
            case None => ""
          }
        } + ")"
      }
      
      def xml : Node =
        <a href={ url } title={ title.getOrElse(null) }>{ childrenXML }</a>
      
      // See the Link toString, hashCode, equals implementations
    }

### `IndirectLink`

Indirect links are tied to `LinkDefinition` elements, which are a special `Block`
type. (The link definitions can't be found in the middle of a paragraph.)

    // In knockoff/IndirectLink.scala
    package knockoff
    
    class IndirectLink(
      children        : SpanSeq,
      val definition  : LinkDefinition
    )
    extends Link( children, definition.url, definition.title )
    with    ComplexSpan {
        
      override def markdown = "[" + childrenMarkdown + "][" + definition.id + "]"
        
      // See the IndirectLink toString, hashCode, equals implementations        
    }

### `ImageLink` and `IndirectImageLink`

Image links are standard link references prefixed with an exclamation mark `!`. The
image aspect is done via this trait:

    // In knockoff/ImageSpan.scala
    package knockoff
    
    import scala.xml.Node
    
    trait ImageSpan extends Link {
      override def markdown = "!" + super.markdown
      
      override def xml : Node = <img
        src={ url }
        title={ title.getOrElse(null) }
        alt={ childrenXML.text }
      ></img>
    }

We then the actual classes using a mixin.

#### `ImageLink`

    // In knockoff/ImageLink.scala
    package knockoff
    
    import scala.xml.Node
    
    class ImageLink(
      children  : SpanSeq,
      url       : String,
      title     : Option[ String ]
    )
    extends Link( children, url, title )
    with    ImageSpan {
      // See the ImageLink toString, hashCode, equals implementations
    }

#### `IndirectImageLink`

    // In knockoff/IndirectImageLink.scala
    package knockoff

    import scala.xml.Node
    
    class IndirectImageLink(
      children    : SpanSeq,
      definition  : LinkDefinition
    )
    extends IndirectLink( children, definition )
    with    ImageSpan {
      // See the IndirectImageLink toString, hashCode, equals implementations
    }


## Why No Case Classes? ##

Each of these classes are __not__ case classes, as I expect that people may want to
override them, to alter HTML output, for example. The [`ElementFactory`][1] can
then be overridden to generate the right output.

This has resulted in a wee bit of duplication (boo), now made referencable at in
this location... (yay?)

#### `Text.hashCode/equals`

    // The Text toString, hashCode, equals implementations
    override def toString = "Text(" + content + ")"
    
    override def hashCode : Int = content.hashCode
        
    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : Text => t.canEqual( this ) && ( t.content == content )
      case _ => false
    }
        
    def canEqual( t : Text ) : Boolean = t.getClass == getClass

#### `HTMLSpan.hashCode/equals`

    // The HTMLSpan toString, hashCode, equals implementations
    override def toString = "HTMLSpan(" + content + ")"
    
    override def hashCode : Int = content.hashCode
        
    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : HTMLSpan => t.canEqual( this ) && ( t.content == content )
      case _ => false
    }
        
    def canEqual( t : HTMLSpan ) : Boolean = t.getClass == getClass

#### `CodeSpan.hashCode/equals`

    // The CodeSpan toString, hashCode, equals implementations
    override def toString = "CodeSpan(" + content + ")"
    
    override def hashCode : Int = content.hashCode
        
    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : CodeSpan => t.canEqual( this ) && ( t.content == content )
      case _ => false
    }
        
    def canEqual( t : CodeSpan ) : Boolean = t.getClass == getClass

#### `Strong.hashCode/equals`

    // The Strong toString, hashCode, equals implementations
    override def toString = "Strong(" + markdown + ")"

    override def hashCode : Int =
      41 + ( (3 /: children)( (sum, child) => 41 + sum + 3 * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : Strong => t.canEqual( this ) && ( t.children sameElements children )
      case _ => false
    }

    def canEqual( s : Strong ) : Boolean = s.getClass == getClass

#### `Emphasis.hashCode/equals`

    // The Emphasis toString, hashCode, equals implementations
    override def toString = "Emphasis(" + markdown + ")"

    override def hashCode : Int =
      43 + ( (3 /: children)( (sum, child) => 43 + sum + 3 * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : Emphasis => t.canEqual( this ) && ( t.children sameElements children )
      case _ => false
    }

    def canEqual( s : Emphasis ) : Boolean = s.getClass == getClass

#### `Link.hashCode/equals`

    // The Link toString, hashCode, equals implementations
    override def toString = "Link(" + markdown + ")"

    override def hashCode : Int = {
      ( 43 + ( (3 /: children)( (sum, child) => 43 + sum + 3 * child.hashCode ) ) ) +
      ( 43 + url.hashCode ) +
      ( 43 + title.hashCode )
    }

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : Link => ( t.canEqual( this ) ) && ( this sameElements t )
      case _ => false
    }

    def canEqual( s : Link ) : Boolean = s.getClass == getClass
    
    def sameElements( l : Link ) : Boolean = {
      ( l.children sameElements children ) &&
      ( url == l.url ) &&
      ( title == l.title )
    }


#### `IndirectLink.hashCode/equals`

    // The IndirectLink toString, hashCode, equals implementations
    override def toString = "IndirectLink(" + markdown + ")"

    override def hashCode : Int =
      41 + ( (7 /: children)( (sum, child) => 41 + sum + 7 * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : IndirectLink => ( t.canEqual( this ) ) && ( this sameElements t )
      case _ => false
    }

    def canEqual( s : IndirectLink ) : Boolean = s.getClass == getClass

#### `ImageLink toString/hashCode/equals`

    // The ImageLink toString, hashCode, equals implementations
    override def toString = "ImageLink(" + markdown + ")"

    override def hashCode : Int =
      37 + ( (13 /: children)( (sum, child) => 37 + sum + 13 * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : ImageLink => t.canEqual( this ) && ( this sameElements t )
      case _ => false
    }

    def canEqual( s : ImageLink ) : Boolean = s.getClass == getClass

#### `IndirectImageLink.hashCode/equals`

    // The IndirectImageLink toString, hashCode, equals implementations
    override def toString = "IndirectImageLink(" + markdown + ")"

    override def hashCode : Int = {
      41 + ( (11 /: children){
        (sum, child) => 41 + sum + 11 * child.hashCode
      } )
    }

    override def equals( rhs : Any ) : Boolean = rhs match {
      case t : IndirectImageLink =>
        ( t.canEqual( this ) ) && ( this sameElements t )
      case _ => false
    }

    def canEqual( s : IndirectImageLink ) : Boolean = s.getClass == getClass


[1]: 04-ElementFactory.html