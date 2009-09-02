Spanning Elements
=================

## `Span` ##

Marks a string to become the basic building block of the Markdown tree.

Each Span is also a sequence of other Spans - some elements, like a link definition,
can have part of it's description be code.

    // In knockoff2/Span.scala
    package com.tristanhunt.knockoff2
    
    import scala.xml.Node
    
    trait Span extends SpanSeq {
        def markdown : String
        def xml : Node
    }

## `SpanSeq` ##

Each `Block` is composed of these.

    // In knockoff2/SpanSeq.scala
    package com.tristanhunt.knockoff2
    
    trait SpanSeq extends Seq[ Span ] {
        def theSeq : Seq[ Span ]
        override def length : Int = theSeq.length
        override def elements = theSeq.elements
        override def apply( ii : Int ) = theSeq(ii)
    }

A simpler version is for the common case, where the span does not actually contain
other spans.

    // In knockoff2/SimpleSpan.scala
    package com.tristanhunt.knockoff2
    
    trait SimpleSpan extends Span {
        def theSeq = List( this )
    }

## `Text` ##

The most basic Span element that contains no other markup information.

    // In knockoff2/Text.scala
    package com.tristanhunt.knockoff2
    
    import scala.xml.{ Node, Text => XMLText }
    
    case class Text( val content : String ) extends SimpleSpan {
        def markdown = content
        def xml : Node = XMLText( content )
        override def toString = "Text(" + content + ")"
    }
