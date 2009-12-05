# `ColoredLogger` #

Useful for testing, not racism.

    // In com/tristanhunt/knockoff/ColoredLogger.scala
    package com.tristanhunt.knockoff
    
    import scala.util.logging.Logged
    
    trait ColoredLogger extends Logged {
      override def log( s : String ) {
        println( Console.GREEN + s + Console.RESET )
      }
    }
