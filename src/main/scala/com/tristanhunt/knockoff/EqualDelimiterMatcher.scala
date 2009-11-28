package com.tristanhunt.knockoff

trait EqualDelimiterMatcher { self : SpanConverter with StringExtras =>
  
  /**
    @param delim The delimiter string to match the next 2 sequences of.
    @param toSpanMatch Factory to create the actual SpanMatch.
    @param recursive If you want the contained element to be reconverted.
  */
  def matchEqualDelimiters( source : String )(
    delim       : String,
    toSpanMatch : ( Int, Option[ Text ], Span, Option[ String ] ) => SpanMatch,
    recursive   : Boolean,
    escape      : Option[ Char ]
  ) : Option[ SpanMatch ] = {
    source.nextNIndicesOf( 2, delim, escape ) match {
      case List( start, end ) => {
        if ( start + delim.length >= end ) return None
        val contained = source.substring( start + delim.length, end )
        val content = {
          if ( recursive ) convert( contained, Nil )
          else elementFactory.text( contained )
        }
        Some(
          toSpanMatch(
            start,
            source.substringOption( 0, start ).map( elementFactory.text ),
            content,
            source.substringOption( end + delim.length, source.length )
          )
        )
      }
      case _ => None
    }
  }
}
