`ElementFactory`
================

Defines constructor methods used to build each of the elements in knockoff. This
can then be easily overriden by specialized versions of the block or span elements,
so that the output `BlockSeq` might render things a bit differently, for example.

    // In knockoff2/ElementFactory.scala
    // See the ElementFactory package and imports
    
    trait ElementFactory {

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
        
        def pos( s : Int, e : Int, o : Source ) = Position( s, e, o )
        
        /** The "empty position" - only useful for ignoring this for tests. */
        def emptyPos = pos( 0, 0, Source.fromString("") )
    }

I used heavy abbreviation in this class in order to draw focus to the types.

#### `ElementFactory` - Package and Imports

    // The ElementFactory package and imports
    package knockoff2
    
    import scala.io.Source