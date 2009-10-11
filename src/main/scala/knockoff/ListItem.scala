package knockoff

import scala.util.parsing.input.Position
import scala.xml.Node

abstract class ListItem(
  val items    : Seq[ Block ],
  val position : Position
)
extends ComplexBlock {
  
  val children = new GroupBlock( items )

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
      childrenXML 
    else
      children.first.span.toXML
  }</li>
  
  def xml : Node = xml( isComplex )
  
  def isComplex = items.length > 1
  
  def + ( block : Block ) : ListItem
  
  // See the ListItem toString, equals, hashCode implementation
}
