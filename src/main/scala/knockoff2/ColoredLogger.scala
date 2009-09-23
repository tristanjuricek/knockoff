package knockoff2

import scala.util.logging.Logged

trait ColoredLogger extends Logged {
  override def log( s : String ) {
    println( Console.GREEN + s + Console.RESET )
  }
}
