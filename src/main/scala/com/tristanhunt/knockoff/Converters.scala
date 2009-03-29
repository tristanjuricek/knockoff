package com.tristanhunt.knockoff

import xml.{Node, NodeBuffer}

/**
 * Converts things to XHTML from our object model. Built in a way to allow simple overriding and 
 * customization.
 */
object BlockConverter {

    import SpanConverter.span

    /**
     * Dispatch to more explicit methods.
     */
    def toXML(block:Block):Node = {
        block match {
            case p:Paragraph            => toParagraphXML(p)
            case h:Header               => toHeaderXML(h)
            case b:Blockquote           => toBlockquoteXML(b)
            case h:HTMLBlock            => SpanConverter.toHTMLXML(h.html)
            case c:CodeBlock            => toCodeBlockXML(c)
            case r:HorizontalRule       => toHorizontalRuleXML(r)
            case l:UnorderedBlockList   => toUnorderedBlockListXML(l)
            case l:OrderedBlockList     => toOrderedBlockListXML(l)
        }
    }
    
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
        <hr/>
    
    def toUnorderedBlockListXML(ublst:UnorderedBlockList):Node =
        <ul>{ublst.items.map(toListItemXML)}</ul>

    def toOrderedBlockListXML(oblst:OrderedBlockList):Node =
        <ol>{oblst.items.map(toListItemXML)}</ol>
    
    def toListItemXML(b:Block):Node =
        <li>{span(b.nads)}</li>

    def convert(blocks:List[Block]):NodeBuffer = {
        val nb = new xml.NodeBuffer
        blocks.foreach(nb &+ toXML(_))
        nb
    }
}


/**
 * More overridable conversion methods.
 */
object SpanConverter {
 
    def span(nads:List[Nad]):NodeBuffer = {
        val nb = new xml.NodeBuffer
        nads.foreach(nb &+ toXML(_))
        nb
    }
    
    def toXML(nad:Nad):Node = {
        nad match {
            case t:Text         => toTextXML(t)
            case h:HTML         => toHTMLXML(h)
            case s:Strong       => toStrongXML(s)
            case e:Emphasis     => toEmphasisXML(e)
            case l:Link         => toLinkXML(l)
            case i:ImageLink    => toImageLinkXML(i)
        }
    }
    
    def toTextXML(t:Text):Node = xml.Text(t.value)
    
    def toHTMLXML(h:HTML):Node = xml.Unparsed(h.value)
    
    def toStrongXML(s:Strong):Node = <strong>{span(s.nads)}</strong>
    
    def toEmphasisXML(e:Emphasis):Node = <em>{span(e.nads)}</em>
    
    def toLinkXML(l:Link):Node =
        <a href={l.url} title={if (l.title == "") null else l.title}>{span(l.nads)}</a>
    
    def toImageLinkXML(i:ImageLink):Node =
        <img src={i.url} title={if (i.title == "") null else i.title}>{span(i.nads)}</img>
}