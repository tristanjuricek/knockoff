package knockoff2

import scala.xml.{ Node, Unparsed }
import scala.io.Source
import scala.util.parsing.input.Position

class   CodeBlock( val text : Text, val position : Position )
extends SimpleBlock {

  def this( preformatted : String, position : Position ) =
    this( new Text( preformatted ), position )

  val span = text

  val preformatted = text.markdown
  
  lazy val preformattedLines =
    Source.fromString( preformatted ).getLines
  
  def markdown =
    preformattedLines.map{ line =>  "    " + line }.mkString("")
      
  def xml : Node = <pre><code>{ Unparsed( preformatted ) }</code></pre>
  
  override def toString = "CodeBlock(" + preformatted + ")"
  
  override def hashCode : Int = preformatted.hashCode
  
  override def equals( rhs : Any ) : Boolean = rhs match {
    case t : CodeBlock => t.canEqual( this ) && ( this sameElements t )
    case _ => false
  }
  
  def sameElements( cb : CodeBlock ) : Boolean = {
    ( cb.preformatted == preformatted ) &&
    ( cb.position == position )
  }
  
  def canEqual( t : CodeBlock ) : Boolean = t.getClass == getClass
}
