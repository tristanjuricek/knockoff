Spanning Elements
=================

## `Span` ##

Marks a string to become the basic building block of the Markdown tree.

Each Span is also a sequence of other Spans - some elements, like a link definition,
can have part of it's description be code.

    // In knockoff2/Span.scala
    package knockoff2
    
    import scala.xml.Node
    
    trait Span extends SpanSeq {
        def markdown : String
        def xml : Node
    }

## `SpanSeq` ##

Each `Block` is composed of these.

    // In knockoff2/SpanSeq.scala
    package knockoff2
    
    trait SpanSeq extends Seq[ Span ] {
        def theSeq : Seq[ Span ]
        override def length : Int = theSeq.length
        override def elements = theSeq.elements
        override def apply( ii : Int ) = theSeq(ii)
    }

A simpler version is for the common case, where the span does not actually contain
other spans.

    // In knockoff2/SimpleSpan.scala
    package knockoff2
    
    trait SimpleSpan extends Span {
        def theSeq = List( this )
    }

The other, complex case is where a span contains a straight list of children.

    // In knockoff2/ComplexSpan.scala
    package knockoff2
    
    abstract class ComplexSpan( val children : List[ Span ] ) extends Span {
        def theSeq = children
        def childrenMarkdown = children.map( _.markdown ).mkString("")
        def childrenXML = children.map( _.xml )
    }

## `Text` ##

The most basic Span element that contains no other markup information.

    // In knockoff2/Text.scala
    package knockoff2
    
    import scala.xml.{ Node, Text => XMLText }
    
    class Text( val content : String ) extends SimpleSpan {

        def markdown = content

        def xml : Node = XMLText( content )

        // See the Text toString, hashCode, equals implementations
    }

## `HTMLSpan` ##

These sequences are found inside of blocks, but still mean "just pass it on".

    // In knockoff2/HTMLSpan.scala
    package knockoff2
    
    import scala.xml.{ Node, Unparsed }
    
    class HTMLSpan( val content : String ) extends SimpleSpan {

        def markdown = content

        def xml : Node = Unparsed( content )

        // See the HTMLSpan toString, hashCode, equals implementations       
    }

## `CodeSpan` ##

These are usually represented by inline `<code>` blocks in paragraph text. This is
not to be confused with `CodeBlock` - a `CodeBlock` does not contain a `CodeSpan`.

    // In knockoff2/CodeSpan.scala
    package knockoff2
    
    import scala.xml.Node
    
    class CodeSpan( val content : String ) extends SimpleSpan {

        def markdown = content

        def xml : Node = <code>{ content }</code>

        // See the CodeSpan toString, hashCode, equals implementations       
    }

## `Strong` ##

These emphasize other spans, usually with `<strong>` tags.

    // In knockoff2/Strong.scala
    package knockoff2
    
    import scala.xml.Node

    class Strong( children : List[ Span ] ) extends ComplexSpan( children ) {
        
        def markdown = "**" + childrenMarkdown + "**"
        
        def xml : Node = <strong>{ childrenXML }</strong>
        
        // See the Strong toString, hashCode, equals implementations
    }

## `Emphasis` ##

Wraps other spans with `<em>` tags.

    // In knockoff2/Strong.scala
    package knockoff2

    import scala.xml.Node

    class Emphasis( children : List[ Span ] ) extends ComplexSpan( children ) {

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

    // In knockoff2/Link.scala
    package knockoff2

    import scala.xml.Node

    class Link(
        children    : List[ Span ],
        val url     : String,
        val title   : Option[ String ]
    )
    extends ComplexSpan( children ) {
        
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
        41 + ( (3 /: children)( (sum, child) => sum * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : Strong => t.canEqual( this ) && ( t.children sameElements children )
        case _ => false
    }

    def canEqual( s : Strong ) : Boolean = s.getClass == getClass

#### `Emphasis.hashCode/equals`

    // The Emphasis toString, hashCode, equals implementations
    override def toString = "Emphasis(" + markdown + ")"

    override def hashCode : Int =
        41 + ( (3 /: children)( (sum, child) => sum * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : Emphasis => t.canEqual( this ) && ( t.children sameElements children )
        case _ => false
    }

    def canEqual( s : Emphasis ) : Boolean = s.getClass == getClass

#### `Link.hashCode/equals`

    // The Link toString, hashCode, equals implementations
    override def toString = "Link(" + markdown + ")"

    override def hashCode : Int =
        41 + ( (3 /: children)( (sum, child) => sum * child.hashCode ) )

    override def equals( rhs : Any ) : Boolean = rhs match {
        case t : Link => t.canEqual( this ) && ( t.children sameElements children )
        case _ => false
    }

    def canEqual( s : Link ) : Boolean = s.getClass == getClass


[1]: 04-ElementFactory.html