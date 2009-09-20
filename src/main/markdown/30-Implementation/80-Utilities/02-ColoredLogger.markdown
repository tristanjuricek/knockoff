# `ColoredLogger` #

Useful for testing, not racism.

    // In knockoff2/ColoredLogger.scala
    package knockoff2
    
    import scala.util.logging.Logged
    
    trait ColoredLogger extends Logged {
      override def log( s : String ) {
        println( Console.GREEN + s + Console.RESET )
      }
    }