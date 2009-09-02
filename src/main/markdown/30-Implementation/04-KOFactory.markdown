`KOFactory`
===========

Generates the various Knockoff elements based primarily on the `SpanSeq`,
though other information learned from parsing is usually passed on as well.

    // In knockoff2/KOFactory.scala
    package com.tristanhunt.knockoff2
    
    trait KOFactory {
        
        def Paragraph( spans : SpanSeq, position : Position ) : Paragraph = {
            new Paragraph(
                spans,
                spans.mkString(""),
                <p>{ spans.mkString("") }</p>,
                position
            )
        }
        
        def Paragraph( textContent : String, position : Position ) : Paragraph =
            Paragraph( Text( textContent ), position )
                
        def Header( level : Int, spans : SpanSeq, position : Position ) : Header = {
            new Header(
                level,
                spans,
                "# " + spans.mkString("") + " #",
                <h1>{ spans.mkString("") }</h1>,
                position
            )
        }
        
        def Header( level : Int, content : String, position : Position ) : Header =
            Header( level, Text( content ), position )
    }
