# Span Conversion #

Our little spanning matching system is broken up into a tail-recursive system that
slowly puts together our strings by:

1. Trying out all alternatives of the next significant spanning element from the
current point.

2. Picking the best match based on earliest first location.

3. Processing current content if it can.

4. Processing the rest of the tail content.

This is initiated by the `SpanConverterFactory`, which sets up a converter, and
creates a bunch of mixins. These mixins are really only intended to

## `SpanConverterFactory` ##

Basically, it just configures a converter routine with the link definitions that are
found elsewhere in the document. This is the `trait` that is included in the top
level element.

Also included here is the `matchers` method, which is a series of methods to do the
matching replacement by the `SpanConverter`. This allows for convenient overriding
at a high-level, mostly for debugging purposes.

    // In knockoff2/SpanConverterFactory.scala
    package knockoff2
    
    trait SpanConverterFactory extends ElementFactory {
     
        def spanConverter( definitions : Seq[ LinkDefinition ] ) : Chunk => SpanSeq =
            new SpanConverter( definitions, matchers, this )
            
        def matchers : Seq[ SpanMatcher ] = List(
            UnderscoreStrongMatcher,
            AsterixStrongMatcher,
            UnderscoreEmphasisMatcher,
            AsterixEmphasisMatcher
        )
    }

## `SpanConverter` ##

The converter implements the tail-recursive methods for spinning through the
content. Note that this recurses in two directions. One, when we find the next
spanning element, this will call itself to work on the tail, iterating "down" the
string. But on certain elements, the element itself contains a Span, so this
converter configures that matcher to kick off another parsing run on the substring
of that span.

    // In knockoff2/SpanConverter.scala
    package knockoff2
    
    class SpanConverter(
        val definitions : Seq[ LinkDefinition ],
        val matchers    : Seq[ SpanMatcher ],
        val factory     : ElementFactory
    )
    extends Function1[ Chunk, SpanSeq ] {
     
        def apply( chunk : Chunk ) : SpanSeq =
            convert( chunk.content, Nil )( factory )
        
        private def convert(
                content : String,
                current : List[ Span ]
            ) ( implicit
                factory : ElementFactory
            ) : Span = {

            if ( content.isEmpty ) return factory.toSpan( current )

            val textOnly =
                SpanMatch( content.length, None, factory.text( content ), None )

            val bestMatch = ( textOnly /: matchers ){ (current, matcher) =>
                matcher.find( content, convert( _, Nil )( factory ) ) match {
                    case None => current
                    case Some( nextMatch ) => {
                        if ( nextMatch.index < current.index )
                            nextMatch
                        else
                            current
                    }
                }
            }
            
            val updated =
                current ::: bestMatch.before.toList ::: List( bestMatch.current )
            
            bestMatch.after match {
                case None              => factory.toSpan( updated )
                case Some( remaining ) => convert( remaining, updated )
            }
        }
    }


## `SpanMatch` ##

The primary result returned by a `SpanMatcher`. It's `index` will become an ordering
attribute for determining the "best" match.

    // In knockoff2/SpanMatch.scala
    package knockoff2
    
    case class SpanMatch(
        val index   : Int,
        val before  : Option[ Text ],
        val current : Span,
        val after   : Option[ String ]
    )


## `SpanMatcher` ##
    
    // In knockoff2/SpanMatcher.scala
    package knockoff2
    
    trait SpanMatcher {

        def recursive = true
        
        def find(
                str     : String,
                convert : String => Span
            ) ( implicit
                factory : ElementFactory
            ) : Option[ SpanMatch ]
    }


## `Emphasis` Matching ##

Emphasis can start with an underscore `_` or an asterix `*`, and can have embedded
elements. Examples:

    _dude_
    *dude*
    an _emphasized expression `with code` inside!_

So... we have a couple of different objects to do the real work.

### `UnderscoreEmphasisMatcher` ###

    // In knockoff2/UnderscoreEmphasisMatcher.scala
    package knockoff2
    
    object  UnderscoreEmphasisMatcher
    extends EqualDelimiterMatcher(
        "_",
        (i,b,c,a,f) => SpanMatch( i, b, f.em(c), a )
    )

### `AsterixEmphasisMatcher` ###

    // In knockoff2/AsterixEmphasisMatcher.scala
    package knockoff2
    
    object   AsterixEmphasisMatcher
    extends EqualDelimiterMatcher(
        "*",
        (i,b,c,a,f) => SpanMatch( i, b, f.em(c), a )
    )


## `Strong` Matching ##

Like `Emphasis` elements, `Strong` elements use two underscores `__` or asterixes
`**` to figure themselves out.

### `UnderscoreStrongMatcher` ###

    // In knockoff2/UnderscoreStrongMatcher.scala
    package knockoff2
    
    object  UnderscoreStrongMatcher
    extends EqualDelimiterMatcher(
        "__",
        (i,b,c,a,f) => SpanMatch( i, b, f.strong(c), a )
    )

### `AsterixStrongMatcher` ###

    // In knockoff2/AsterixStrongMatcher.scala
    package knockoff2
    
    object  AsterixStrongMatcher
    extends EqualDelimiterMatcher(
        "**",
        (i,b,c,a,f) => SpanMatch( i, b, f.strong(c), a )
    )


## `EqualDelimiterMatcher` ##

    // In knockoff2/EqualDelimiterMatcher.scala
    package knockoff2
    
    class   EqualDelimiterMatcher(
        val delim    : String,
        val newMatch : ( Int, Option[ Text ], Span, Option[ String ], ElementFactory ) => SpanMatch
    )
    extends SpanMatcher
    with    StringExtras {
        
        def find(
                str     : String,
                convert : String => Span
            ) ( implicit
                factory : ElementFactory
            ) : Option[ SpanMatch ] = {
         
            import factory._
         
            str.nextNIndicesOf( 2, delim ) match {
                case List( start, finish ) => {
                    Some( newMatch(
                        start,
                        str.substringOption( 0, start ).map( text ),
                        toSpan( convert( str.substring( start + delim.length, finish ) ) ),
                        str.substringOption( finish + delim.length, str.length ),
                        factory
                    ) )
                }
                case _ => None
            }
        }
    }
