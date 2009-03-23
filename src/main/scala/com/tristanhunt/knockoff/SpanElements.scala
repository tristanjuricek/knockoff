package com.tristanhunt.knockoff

trait Nad {
    def value:String
}

case class Text(val value:String) extends Nad
case class HTML(val value:String) extends Nad

trait Span extends Nad {

    def nads:Seq[Nad]

    def value:String = {
        val sb = new StringBuilder
        nads.foreach(s => sb.append(s.value))
        return sb.toString
    }
}

case class Strong   (val nads:Seq[Nad]) extends Span
case class Emphasis (val nads:Seq[Nad]) extends Span

/**
 * This is a standard link; use a ReferenceLink for the [value][ref] format.
 */
abstract class AbstractLink(val nads:Seq[Nad], val url:String, val title:String) extends Span

class Link(span:Seq[Nad], url:String, title:String) extends AbstractLink(span, url, title) {
 
    override def equals(rhs:Any):Boolean = rhs match {
        case oth:Link => (nads sameElements oth.nads) && url == oth.url && title == oth.title
        case _ => false
    }
    
    override def hashCode:Int =
        41 * (
            41 * (
                41 * {
                    var total = 0
                    span.foreach(total += _.hashCode)
                    total
                }
            ) + url.hashCode
        ) + title.hashCode
    
 
    override def toString:String = "Link(" + span + "," + url + "," + title + ")"
}

object Link {
    
    def apply(span:Seq[Nad], url:String):Link = apply(span, url, "")
    
    def apply(span:Seq[Nad], url:String, title:String):Link = new Link(span, url, title)
}

class ImageLink(span:Seq[Nad], url:String, title:String) extends AbstractLink(span, url, title) {
 
    override def equals(rhs:Any):Boolean = rhs match {
        case oth:ImageLink => (nads sameElements oth.nads) && url == oth.url && title == oth.title
        case _ => false
    }
    
    override def hashCode:Int =
        37 * (
            37 * (
                37 * {
                    var total = 0
                    span.foreach(total += _.hashCode)
                    total
                }
            ) + url.hashCode
        ) + title.hashCode
    
 
    override def toString:String = "ImageLink(" + span + "," + url + "," + title + ")"
}

object ImageLink {
    
    def apply(span:Seq[Nad], url:String):ImageLink = apply(span, url, "")
    
    def apply(span:Seq[Nad], url:String, title:String):ImageLink = new ImageLink(span, url, title)
}

