`ElementFactory`
================

Defines constructor methods used to build each of the elements in knockoff. This
can then be easily overriden by specialized versions of the block or span elements,
so that the output `BlockSeq` might render things a bit differently, for example.

    // In knockoff2/ElementFactory.scala
    // See the ElementFactory package and imports
    
    trait ElementFactory {

        // Block Elements
        
        def para( s : Span, p : Location ) =
            new Paragraph( s, p )
        
        def head( l : Int, s : Span, p : Location ) =
            new Header( l, s, p )
        
        def linkdef( i : String, u : String, t : Option[ String ], p : Location ) =
            new LinkDefinition( i, u, t, p )
        
        def blockquote( c : BlockSeq, p : Location ) : Blockquote =
            new Blockquote( c, p )
        
        def htmlBlock( h : String, p : Location ) : HTMLBlock =
            new HTMLBlock( h, p )
        
        def simpleOL( osis : OrderedSimpleItem * ) : MarkdownList =
            new MarkdownList( true, new GroupBlock( osis ) )
        
        def complexOL( ocis : OrderedComplexItem * ) : MarkdownList =
            new MarkdownList( true, new GroupBlock( ocis ) )
        
        def simpleUL( usis : UnorderedSimpleItem * ) : MarkdownList =
            new MarkdownList( false, new GroupBlock( usis ) )

        def complexUL( ucis : UnorderedComplexItem * ) : MarkdownList =
            new MarkdownList( false, new GroupBlock( ucis ) )
        
        def osi( s : Span, p : Location ) : OrderedSimpleItem =
            new OrderedSimpleItem( s, p )
        
        def oci( b : BlockSeq, p : Location ) : OrderedComplexItem =
            new OrderedComplexItem( b, p )

        def usi( s : Span, p : Location ) : UnorderedSimpleItem =
            new UnorderedSimpleItem( s, p )

        def uci( b : BlockSeq, p : Location ) : UnorderedComplexItem =
            new UnorderedComplexItem( b, p )
        
        
        // Span Elements
        
        def text( c : String ) = new Text( c )
        
        /** A shorthand for text (popular with my tests) */
        def t( c : String ) = text( c )
        
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
        
        // Special
        
        def loc( s : Int, e : Int, o : Source ) : Location =
            loc( s, e, 0, o )
        
        def loc( s : Int, e : Int, c : Int, o : Source ) : Location =
            Location( s, e, c, o )

    }

I used heavy abbreviation in this class in order to draw focus to the types.

#### `ElementFactory` - Package and Imports

    // The ElementFactory package and imports
    package knockoff2
    
    import scala.io.Source