`Position`
==========

A somewhat special utility that allows each `Block` to know how it was
generated from the source.

    // In knockoff2/Position.scala
    package com.tristanhunt.knockoff2
    
    case class Position (
        val linesStart : Int,
        val linesEnd : Int
        // val source : Source
    )

#### Position - Package And Imports

    // The Position package and imports
    package com.tristanhunt.knockoff2