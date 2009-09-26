`ElementFactory`
================

Defines constructor methods used to build each of the elements in knockoff. This
can then be easily overriden by specialized versions of the block or span elements,
so that the output `BlockSeq` might render things a bit differently, for example.

## `HasElementFactory`

For any `Discounter`, there should really only be one, configurable,
`ElementFactory` instance. And for the things that need to fetch that instance,
there is the `HasElementFactory` trait.

    // In knockoff2/HasElementFactory.scala
    package knockoff2
    
    trait HasElementFactory {
    
        def elementFactory : ElementFactory = defaultElementFactory
        
        private val defaultElementFactory = new ElementFactory
    }

Note that the top-level element, `Discounter`, maintains this reference as well. So
customizing the `ElementFactory` is pretty simple. You create a subtype of
`ElementFactory`, then override that in your custom `Discounter` instance:

    // Your custom ElementFactory
    
    // You probably want something like this to bind in prettification in your
    // HTML documents...
    class MyCodeSpan( content : String ) extends CodeSpan( content ) {
      override def = <code class="myCodeClass">{ content }</code>
    }
    
    class MyElementFactory extends ElementFactory {
      override def codeSpan( c : String ) = new MyCodeSpan( c )
    }
    
    class MyDiscounter extends Discounter {
      override val elementFactory = new MyElementFactory
    }

### `ElementFactory`

    // In knockoff2/ElementFactory.scala
    // See the ElementFactory package and imports
    
    class ElementFactory {

      // Block Elements
      
      def para( s : Span, p : Position ) =
        new Paragraph( s, p )
      
      def head( l : Int, s : Span, p : Position ) =
        new Header( l, s, p )
      
      def linkdef( i : String, u : String, t : Option[ String ], p : Position ) =
        new LinkDefinition( i, u, t, p )
      
      def blockquote( c : BlockSeq, p : Position ) : Blockquote =
        new Blockquote( c, p )
      
      def htmlBlock( h : String, p : Position ) : HTMLBlock =
        new HTMLBlock( h, p )
      
      def simpleOL( osis : OrderedSimpleItem * ) : MarkdownList =
        new MarkdownList( true, new GroupBlock( osis ) )
      
      def complexOL( ocis : OrderedComplexItem * ) : MarkdownList =
        new MarkdownList( true, new GroupBlock( ocis ) )
      
      def simpleUL( usis : UnorderedSimpleItem * ) : MarkdownList =
        new MarkdownList( false, new GroupBlock( usis ) )

      def complexUL( ucis : UnorderedComplexItem * ) : MarkdownList =
        new MarkdownList( false, new GroupBlock( ucis ) )
      
      def osi( s : Span, p : Position ) : OrderedSimpleItem =
        new OrderedSimpleItem( s, p )
      
      def oci( b : BlockSeq, p : Position ) : OrderedComplexItem =
        new OrderedComplexItem( b, p )

      def usi( s : Span, p : Position ) : UnorderedSimpleItem =
        new UnorderedSimpleItem( s, p )

      def uci( b : BlockSeq, p : Position ) : UnorderedComplexItem =
        new UnorderedComplexItem( b, p )
      
      
      // Span Elements
      
      def text( c : String ) = new Text( c )
      
      /** A shorthand for text (popular with my tests) */
      def t( c : String ) = text( c )
      
      def em( s : SpanSeq ) : Emphasis =
        new Emphasis( s )
      
      def strong( s : SpanSeq ) : Strong =
        new Strong( s )
      
      def link( c : SpanSeq, u : String, t : Option[ String ] ) : Link =
        new Link( c, u, t )
      
      def link( c : SpanSeq, u : String ) : Link = link( c, u, None )
      
      def link( c : SpanSeq, ld : LinkDefinition ) : IndirectLink =
        new IndirectLink( c, ld )
      
      def ilink( c : SpanSeq, u : String, t : Option[ String ] ) : ImageLink =
        new ImageLink( c, u, t )
      
      def ilink( c : SpanSeq, u : String ) : ImageLink = ilink( c, u )
      
      def ilink( c : SpanSeq, ld : LinkDefinition ) : IndirectImageLink =
        new IndirectImageLink( c, ld )
      
      def codeSpan( c : String ) : CodeSpan =
        new CodeSpan( c )
      
      def htmlSpan( h : String ) : HTMLSpan =
        new HTMLSpan( h )
      
      
      // Uh... fun with my object model?
      
      def toSpan( seq : Seq[ Span ] ) : Span =
        if ( seq.length == 1 ) seq.first else new GroupSpan( seq )
    }

I used heavy abbreviation in this class in order to draw focus to the types.

#### `ElementFactory` - Package and Imports

    // The ElementFactory package and imports
    package knockoff2
    
    import scala.io.Source
    import scala.util.parsing.input.Position