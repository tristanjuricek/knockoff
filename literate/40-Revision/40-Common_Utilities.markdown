# Common Utilities #



## Logging Output ##

The primary reader and writer traits should use the `logParsingError` to receive
status messages from the parsing system.

    // In com/tristanhunt/knockoff2/LoggedParser.scala
    package com.tristanhunt.knockoff2
    
    import scala.util.logging.{ Logged }
    import scala.util.parsing.input.{ Reader }

    trait LoggedParser extends Logged {
     
      def logParsingError[T]( msg : String, reader : Reader[T] ) : Unit =
        log( msg )
    }



## Strings With Parsing Position ##

A few of the parsers are used to pass the position on, which is typically ignored
by the default set of regular expression parsers.

    // In com/tristanhunt/knockoff2/StringPositional.scala
    package com.tristanhunt.knockoff2
    
    import scala.util.parsing.input.{ Position, Positional }
    
    case class StringPositional( val str : String ) extends Positional
