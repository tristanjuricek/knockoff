package com.tristanhunt.knockoff2

import scala.util.logging.{ Logged }
import scala.util.parsing.input.{ Reader }

trait LoggedParser extends Logged {
 
  def logParsingError[T]( msg : String, reader : Reader[T] ) : Unit =
    log( msg )
}
