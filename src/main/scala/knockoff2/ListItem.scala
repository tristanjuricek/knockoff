package knockoff2

import scala.util.parsing.input.Position
import scala.xml.Node

abstract class ListItem(
  val children : BlockSeq,
  val position : Position
)
extends ComplexBlock {

  def itemPrefix : String
  
  def markdown : String = {
    if ( children.isEmpty ) return ""

    return (
      ( itemPrefix + children.first.markdown + "  \n" ) +
      children.drop(1).map( "    " + _.markdown + "  \n" ).mkString( "" )
    )
  }
  
  def xml( complex : Boolean ) : Node = <li>{
    if ( isComplex )
      children.first.span.toXML
    else
      childrenXML 
  }</li>
  
  def xml : Node = xml( isComplex )
  
  def isComplex = children.length > 1
  
  // See the ListItem toString, equals, hashCode implementation
}
