package com.tristanhunt.knockoff

/**
 * At the very end, we have Text contents that are made out of spans.
 */
trait Nad {
    def value:String
}

trait Spanned extends Nad {
    
    def span:Seq[Nad]
    
    override def value:String = {
        val sb = new StringBuilder
        span.foreach(n => sb.append(n.value).append(' '))
        sb.toString
    }
}

case class Text(val value:String) extends Nad

/**
 * Note that this should generally parse, but we only basically match until we find the closing tag.
 * This means that when you break into HTML, you do it the entire way, because no entitizing will
 * be done on the content.
 */ 
case class HTML(val value:String) extends Nad

/**
 * This is a standard link; use a ReferenceLink for the [value][ref] format.
 */
abstract case class AbstractLink(val span:Seq[Nad], val url:String, val title:String)
extends Spanned with Nad {
    
    assume(span != null)
    assume(url != null)
    assume(title != null)
}

class Link(span:Seq[Nad], url:String, title:String) extends AbstractLink(span, url, title) {
    
    def this(span:Seq[Nad], url:String) = this(span, url, "")
}

class LinkDefinition(span:Seq[Nad], url:String, title:String)
extends AbstractLink(span, url, title) {
    
    def this(span:Seq[Nad], url:String) = this(span, url, "")
}

class ImageLink(span:Seq[Nad], url:String, title:String)
extends AbstractLink(span, url, title) {

    def this(span:Seq[Nad], url:String) = this(span, url, "")
}

case class ReferenceLink(val span:Seq[Nad], val reference:String) extends Spanned {
    
    assume(span != null)
    assume(reference != null)
}