package com.tristanhunt.knockoff

import xml.{Node, NodeBuffer}

trait BlockConverter {
    
    /**
     * Dispatch to more explicit methods.
     */
    def toXML(block:Block):Node  = {
         block match {
             case p:Paragraph            => toParagraphXML(p)
             case h:Header               => toHeaderXML(h)
             case b:Blockquote           => toBlockquoteXML(b)
             case h:HTMLBlock            => SpanConverter.current.toHTMLXML(h.html)
             case c:CodeBlock            => toCodeBlockXML(c)
             case r:HorizontalRule       => toHorizontalRuleXML(r)
             case l:UnorderedBlockList   => toUnorderedBlockListXML(l)
             case l:OrderedBlockList     => toOrderedBlockListXML(l)
             case l:UnorderedComplexBlockList   => toUnorderedComplexBlockListXML(l)
             case l:OrderedComplexBlockList     => toOrderedComplexBlockListXML(l)
         }
     }
    
    // Methods intended to be overridden
    
    def toParagraphXML(paragraph:Paragraph):Node
    
    def toHeaderXML(header:Header):Node
    
    def toBlockquoteXML(b:Blockquote):Node
    
    def toCodeBlockXML(c:CodeBlock):Node

    def toHorizontalRuleXML(r:HorizontalRule):Node
    
    def toUnorderedBlockListXML(ublst:UnorderedBlockList):Node

    def toOrderedBlockListXML(oblst:OrderedBlockList):Node

    def toUnorderedComplexBlockListXML(ublst:UnorderedComplexBlockList):Node

    def toOrderedComplexBlockListXML(oblst:OrderedComplexBlockList):Node
    
    def toListItemXML(b:Block):Node

    def convert(blocks:List[Block]):NodeBuffer
}

class DefaultBlockConverter extends BlockConverter {
    
    import SpanConverter.span
    
    def toParagraphXML(paragraph:Paragraph):Node = <p>{span(paragraph.nads)}</p>

    def toHeaderXML(header:Header):Node = {
       header match {
           case Header(nads, 1)    => <h1>{span(nads)}</h1>
           case Header(nads, 2)    => <h2>{span(nads)}</h2>
           case Header(nads, 3)    => <h3>{span(nads)}</h3>
           case Header(nads, 4)    => <h4>{span(nads)}</h4>
           case Header(nads, 5)    => <h5>{span(nads)}</h5>
           case Header(nads, 6)    => <h6>{span(nads)}</h6>
       }
    }

    def toBlockquoteXML(b:Blockquote):Node =
       <blockquote>{convert(b.blocks)}</blockquote>

    def toCodeBlockXML(c:CodeBlock):Node =
       <pre><code>{c.preformatted.value}</code></pre>

    def toHorizontalRuleXML(r:HorizontalRule):Node =
       <hr />

    def toUnorderedBlockListXML(ublst:UnorderedBlockList):Node =
       <ul>{ublst.items.map(toListItemXML)}</ul>

    def toOrderedBlockListXML(oblst:OrderedBlockList):Node =
       <ol>{oblst.items.map(toListItemXML)}</ol>
       
    def toUnorderedComplexBlockListXML( list:UnorderedComplexBlockList ):Node =
        <ul>{ list.items.map( toListItemXML ) }</ul>

    def toOrderedComplexBlockListXML( list:OrderedComplexBlockList ):Node =
        <ol>{ list.items.map( toListItemXML ) }</ol>

    def toListItemXML(b:Block):Node =
       <li>{ span( b.nads ) }</li>
       
    def toListItemXML( blocks : Seq[ Block ] ) : Node =
        <li>{ convert( blocks.toList ) }</li>

    def convert(blocks:List[Block]):NodeBuffer = {
       val nb = new xml.NodeBuffer
       blocks.foreach(nb &+ toXML(_))
       nb
    }
}

/**
 * Converts things to XHTML from our object model. Built in a way to allow simple overriding and 
 * customization.
 */
object BlockConverter {

    /**
     * Enables (simple) customization by empowering people to simply reassign this value when
     * necessary.
     */
    var current :BlockConverter = new DefaultBlockConverter
   
    /**
     * Wrapping method (that's intended to be used) that will convert the Block to a Scala XHTML
     * tree.
     */
    def toXML ( block:Block ) :Node = current.toXML ( block )
    
    /**
     * Constructs a NodeBuffer over the complete list of Blocks (usually, the document 
     * once the entire tree has been parsed.
     */
    def toXML ( blocks :List[Block] ) :NodeBuffer = {
        val nb = new xml.NodeBuffer
        blocks.foreach(nb &+ toXML(_))
        return nb
    }
    
}


trait SpanConverter {
    
    def span(nads:List[Nad]):NodeBuffer = {
        val nb = new xml.NodeBuffer
        nads.foreach(nb &+ toXML(_))
        nb
    }
    
    
    def toXML(nad:Nad):Node = {
        nad match {
            case t:Text         => toTextXML(t)
            case h:HTML         => toHTMLXML(h)
            case c:Code         => toCodeXML(c)
            case s:Strong       => toStrongXML(s)
            case e:Emphasis     => toEmphasisXML(e)
            case l:Link         => toLinkXML(l)
            case i:ImageLink    => toImageLinkXML(i)
        }
    }
    

    val escapeableChars = List(
        "\\", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!", ">"
    )


    def unescape(source:String):String = {
        var buf:String = source
        for ((escaped, unescaped) <- escapeableChars.map(ch => ("\\" + ch, ch)))
            buf = buf.replace(escaped, unescaped)
        buf
    }

    
    def toTextXML(t:Text):Node
    
    def toHTMLXML(h:HTML):Node
    
    def toCodeXML(c:Code):Node
    
    def toStrongXML(s:Strong):Node
    
    def toEmphasisXML(e:Emphasis):Node
    
    def toLinkXML(l:Link):Node
        
    def toImageLinkXML(i:ImageLink):Node
}

class DefaultSpanConverter extends SpanConverter {

    def toTextXML(t:Text):Node = xml.Text(unescape(t.value))
    
    def toHTMLXML(h:HTML):Node = xml.Unparsed(h.value)
    
    def toCodeXML(c:Code):Node = <code>{xml.Text(c.value)}</code>
    
    def toStrongXML(s:Strong):Node = <strong>{span(s.nads)}</strong>
    
    def toEmphasisXML(e:Emphasis):Node = <em>{span(e.nads)}</em>
    
    def toLinkXML(l:Link):Node =
        <a href={l.url} title={if (l.title == "") null else l.title}>{span(l.nads)}</a>
    
    def toImageLinkXML(i:ImageLink):Node =
        <img src={i.url} title={if (i.title == "") null else i.title}>{span(i.nads)}</img>
}


/**
 * More overridable conversion methods.
 */
object SpanConverter {
 
    var current :SpanConverter = new DefaultSpanConverter

    def span ( nads :List[Nad] ) :NodeBuffer = current.span ( nads )
}