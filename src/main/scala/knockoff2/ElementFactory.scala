package knockoff2

import scala.io.Source
import scala.util.parsing.input.Position

class ElementFactory {

  // Block Elements
  
  def para( s : Span, p : Position ) =
    new Paragraph( s, p )
  
  def head( l : Int, s : Span, p : Position ) =
    new Header( l, s, p )
    
  def hr( p : Position ) =
    new HorizontalRule( p )
  
  def linkdef( i : String, u : String, t : Option[ String ], p : Position ) =
    new LinkDefinition( i, u, t, p )
  
  def blockquote( c : BlockSeq, p : Position ) : Blockquote =
    new Blockquote( c, p )
  
  def htmlBlock( h : String, p : Position ) : HTMLBlock =
    new HTMLBlock( h, p )
  
  def simpleOL( osis : OrderedSimpleItem * ) : MarkdownList =
    new OrderedList( new BlockSeq{ def theSeq = osis } )
  
  def complexOL( ocis : OrderedComplexItem * ) : MarkdownList =
    new OrderedList( new BlockSeq{ def theSeq = ocis } )
  
  def simpleUL( usis : UnorderedSimpleItem * ) : MarkdownList =
    new UnorderedList( new BlockSeq{ def theSeq = usis } )

  def complexUL( ucis : UnorderedComplexItem * ) : MarkdownList =
    new UnorderedList( new BlockSeq{ def theSeq = ucis } )
  
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
