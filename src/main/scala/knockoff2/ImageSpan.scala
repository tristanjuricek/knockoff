package knockoff2

import scala.xml.Node

trait ImageSpan extends Link {
    override def markdown = "!" + super.markdown
    
    override def xml : Node = <img
        src={ url }
        title={ title.getOrElse(null) }
        alt={ childrenXML.text }
    ></img>
}
