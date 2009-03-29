package com.tristanhunt.knockoff

trait Nad {
    def value:String
}

case class Text(val value:String) extends Nad
case class HTML(val value:String) extends Nad

trait Span extends Nad {

    def nads:List[Nad]

    def value:String = {
        val sb = new StringBuilder
        nads.foreach(s => sb.append(s.value))
        return sb.toString
    }
}

case class Strong   (val nads:List[Nad]) extends Span
case class Emphasis (val nads:List[Nad]) extends Span

/**
 * 
 */
abstract class AbstractLink(val nads:List[Nad], val url:String, val title:String) extends Span

class Link(nads:List[Nad], url:String, title:String) extends AbstractLink(nads, url, title) {
 
    override def equals(rhs:Any):Boolean = rhs match {
        case oth:Link => (nads sameElements oth.nads) && url == oth.url && title == oth.title
        case _ => false
    }
    
    override def hashCode:Int =
        41 * (
            41 * (
                41 * {
                    var total = 0
                    nads.foreach(total += _.hashCode)
                    total
                }
            ) + url.hashCode
        ) + title.hashCode
    
 
    override def toString:String = "Link(" + nads + "," + url + "," + title + ")"
}

object Link {
    
    def apply(nads:List[Nad], url:String):Link = apply(nads, url, "")
    
    def apply(nads:List[Nad], url:String, title:String):Link = new Link(nads, url, title)
}

class ImageLink(nads:List[Nad], url:String, title:String) extends AbstractLink(nads, url, title) {
 
    override def equals(rhs:Any):Boolean = rhs match {
        case oth:ImageLink => (nads sameElements oth.nads) && url == oth.url && title == oth.title
        case _ => false
    }
    
    override def hashCode:Int =
        37 * (
            37 * (
                37 * {
                    var total = 0
                    nads.foreach(total += _.hashCode)
                    total
                }
            ) + url.hashCode
        ) + title.hashCode
    
 
    override def toString:String = "ImageLink(" + nads + "," + url + "," + title + ")"
}

object ImageLink {
    
    def apply(nads:List[Nad], url:String):ImageLink = apply(nads, url, "")
    
    def apply(nads:List[Nad], url:String, title:String):ImageLink = new ImageLink(nads, url, title)
}

