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
            DoubleCodeMatcher,
            SingleCodeMatcher,
            InlineHTMLMatcher,
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


## `Code` Matching ##

Two varations of code blocks:

    A `normal code` block
    A ``code with a `backtick` inside``

### `DoubleCodeMatcher` ###

    // In knockoff2/DoubleCodeMatcher.scala
    package knockoff2
    
    object DoubleCodeMatcher
    extends CodeDelimiterMatcher(
        "``",
        (i,b,c,a,f) =>
            SpanMatch( i, b, f.codeSpan( c.asInstanceOf[ Text ].content ), a )
    )

### `SingleCodeMatcher` ###

    // In knockoff2/SingleCodeMatcher.scala
    package knockoff2
    
    object SingleCodeMatcher
    extends CodeDelimiterMatcher(
        "`",
        (i,b,c,a,f) =>
            SpanMatch( i, b, f.codeSpan( c.asInstanceOf[ Text ].content ), a )
    )

#### `CodeMatcherSpec`

    // In test knockoff2/SingleCodeMatcherSpec.scala
    package knockoff2
    
    import org.scalatest._
    
    class SingleCodeMatcherSpec extends Spec with SpanConverterFactory {
        describe( "SingleCodeMatcher" ) {
            it( "should parse a couple of single code blocks in text" ) {
                val spans = spanConverter( Nil )(
                    TextChunk("a `code1` and a `code 2`")
                )
                val expected = List(
                    t("a "), codeSpan("code1"), t(" and a "), codeSpan("code 2")
                )
                assert( spans sameElements expected )
            }
        }
    }

### `CodeDelimiterMatcher` ###

    // In knockoff2/CodeDelimiterMatcher.scala
    package knockoff2
    
    class CodeDelimiterMatcher(
        val delim    : String,
        val newMatch : ( Int, Option[ Text ], Span, Option[ String ], ElementFactory ) => SpanMatch
    )
    extends EqualDelimiterMatcher( delim, newMatch ) {
     
        override def find(
                str     : String,
                convert : String => Span
            ) ( implicit
                factory : ElementFactory
            ) : Option[ SpanMatch ] = {
            return super.find( str, source => factory.text( source ) )
        }
    }

## HTML Matching ##

If we find any kind of HTML/XML-like element within the content, and it's not a
single element, we try to find the ending element. If that segment isn't
well-formed, we just ignore the element. The matcher


### `InlineHTMLMatcher`

Any sequences of HTML in content are matched by the `InlineHTMLMatcher`.

    // In knockoff2/InlineHTMLMatcher.scala
    package knockoff2
    
    object InlineHTMLMatcher extends SpanMatcher with StringExtras with ColoredLogger {
        
        val startElement = """<[ ]*([a-zA-Z:_]+)[ \t]*[^>]*?(/?+)>""".r
        
        def find(
                str     : String,
                convert : String => Span
            ) ( implicit
                factory : ElementFactory
            ) : Option[ SpanMatch ] = {
                
            import factory.{ htmlSpan, text }
            
            startElement.findFirstMatchIn( str ).map { open =>
                
                val hasEnd = open.group(2) == "/"

                if ( hasEnd ) {
                    SpanMatch(
                        open.start,
                        open.before.toOption.map( text(_) ),
                        htmlSpan( open.matched ),
                        open.after.toOption
                    )
                } else {
                    hasMatchedClose( str, open.group(1), open.end, 1 ).map {
                        closeAndAfter => SpanMatch(
                            open.start,
                            open.before.toOption.map( text(_) ),
                            htmlSpan( str.substring( open.start, closeAndAfter._1 ) ),
                            closeAndAfter._2.toOption
                        )
                    }.getOrElse( return None )
                }
            }
        }
        
        private def hasMatchedClose(
                source : String,
                tag    : String,
                from   : Int,
                opens  : Int
            ) : Option[ ( Int, CharSequence ) ] = {
            
            val opener = ("(?i)<[ ]*" + tag + "[ \t]*[^>]*?(/?+)*>").r
            val closer = ("(?i)</[ ]*" + tag + "[ ]*>").r
            
            val nextOpen = opener.findFirstMatchIn( source.substring(from) )
            val nextClose = closer.findFirstMatchIn( source.substring(from) )

            if ( ! nextClose.isDefined ) return None
            
            if ( nextOpen.isDefined && ( nextOpen.get.start < nextClose.get.start ) ) {
                hasMatchedClose( source, tag, from + nextOpen.get.end, opens + 1 )
            } else if ( opens > 1 ) {
                hasMatchedClose( source, tag, from + nextClose.get.end, opens - 1 )
            } else {
                Some( ( from + nextClose.get.end, nextClose.get.after ) )
            }
        }
    }


#### `InlineHTMLMatcherSpec`

    // In test knockoff2/InlineHTMLMatcherSpec.scala
    package knockoff2
    
    import org.scalatest._
    import org.scalatest.matchers._
    
    class InlineHTMLMatcherSpec extends Spec with ShouldMatchers with SpanConverterFactory {

        describe("InlineHTMLMatcher") {
         
            it("should find an <a> and an <img>") {
                val spans = spanConverter( Nil )( TextChunk(
                    """with <a href="http://example.com">a link</a> and an """ +
                    """<img src="foo.img"/> ha!"""
                ) )
                
                spans.toList should equal ( List(
                    t("with "),
                    htmlSpan("""<a href="http://example.com">a link</a>"""),
                    t(" and an "),
                    htmlSpan("""<img src="foo.img"/>"""),
                    t(" ha!")
                ) )
            }
            
            it("should wrap a <span> that contains another <span>") {
                val convertedSpans = spanConverter( Nil ){ TextChunk(
                    """a <span class="container">contains <span>something</span>""" +
                    """ else</span> without a problem <br /> !"""
                ) }
                
                convertedSpans.toList should equal { List(
                    t("a "),
                    htmlSpan(
                        """<span class="container">contains """ +
                        """<span>something</span> else</span>"""
                    ),
                    t(" without a problem "),
                    htmlSpan("<br />"),
                    t(" !")
                ) }
            }
        }
    }

### `EntityMatcher`

If we see HTML entity sequences in text, we'll mark that as html. This is to allow
the text to simply "pass through" to the the final content.

    // In knockoff2/EntityMatcher.scala
    package knockoff2
    
    object EntityMatcher extends SpanMatcher with StringExtras {
        override def recursive = false

        val matchEntity = """&\w+;""".r

        def find(
                str     : String,
                convert : String => Span
            ) ( implicit
                factory : ElementFactory
            ) : Option[ SpanMatch ] = {
                
            matchEntity.findFirstMatchIn( str ) match {
                
                case None => None
                
                case Some( entityMatch ) => Some(
                    SpanMatch(
                        entityMatch.start,
                        entityMatch.before.toOption.map( factory.text(_) ),
                        factory.htmlSpan( entityMatch.matched ),
                        entityMatch.after.toOption
                    )
                )
            }
        }
        

    }


## `EqualDelimiterMatcher` ##

Many of the elements are delimited by the identical character sequence on either
side of the text. This does the dirty work of finding those matches, whatever that
character sequence may be.

    // In knockoff2/EqualDelimiterMatcher.scala
    package knockoff2
    
    import scala.util.logging.Logged
    
    class   EqualDelimiterMatcher(
        delim    : String,
        newMatch : ( Int, Option[ Text ], Span, Option[ String ], ElementFactory ) => SpanMatch
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
                        convert( str.substring( start + delim.length, finish ) ),
                        str.substringOption( finish + delim.length, str.length ),
                        factory
                    ) )
                }
                case _ => None
            }
        }
    }
