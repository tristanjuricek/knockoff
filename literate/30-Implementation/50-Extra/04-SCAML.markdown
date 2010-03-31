# SCAML Interpolation #

If you are using something like [Scalate][] as a template system, you may want to
inject some basic [SCAML][] scala code in your documents, treating them like
templates themselves. Right now, this focuses only on interpolated statements, like:

    This is version #{project.lastVersion} 

This is captured in a spanning element:

    // The InterpolatedSCAML class
    case class InterpolatedSCAML( content : String ) extends Span

It's captured within text sequences only. Thus, if you try something like:

    I'm an `#{example}` inside some #{text}.

You'll get the spanning sequence:

    Text("I'm an ")
    CodeSpan("#{example}")
    Text(" inside some ")
    InterpolatedSCAML("#{text}")

We do this by overriding the `Discounter.knockoff` method and splitting up `Text`
once the document is converted.

    // The SCAMLDiscounter trait
    trait SCAMLDiscounter extends Discounter with StringExtras
      with SCAMLXHTMLWriter with SCAMLPlainTextWriter {
      
      override def knockoff( source : CharSequence ) : Seq[Block] =
        super.knockoff( source ).map( convertSCAMLBlock )
      
      private def convertSCAMLBlock( block : Block ) : Block = block match {
        case Paragraph( spans, pos ) =>
          Paragraph( spans.flatMap( convertSCAMLSpan ), pos )
        case Blockquote( children, pos ) =>
          Blockquote( children.map( convertSCAMLBlock ), pos )
        case OrderedList( items ) =>
          OrderedList( items.map( li => OrderedItem( li.children.map(convertSCAMLBlock),
                                                     li.position ) ) )
        case UnorderedList( items ) =>
          UnorderedList( items.map( li => UnorderedItem( li.children.map(convertSCAMLBlock),
                                                         li.position ) ) )
        case _ => block
      }
      
      private def convertSCAMLSpan( span : Span ) : Seq[Span] = span match {
        case text : Text => splitSCAML( text )
        case _ => List(span)
      }
      
      private def splitSCAML( text : Text ) : Seq[Span] = {
        val start = text.content.indexOf("#{")
        if ( -1 < start ) {
          text.content.findBalanced( '{', '}', start + 1 ) match {
            case Some( end ) =>
              val list = List( Text( text.content.substring( 0, start ) ),
                    InterpolatedSCAML( text.content.substring( start, end + 1 ) ) )
              if ( end + 1 < text.content.length )
                return list ++ splitSCAML( Text(text.content.substring( end + 1 )) )
              else
                return list
            case None => return List( text )
          }
        } else {
          return List( text )
        }
      }
    }
    
    // The SCAML parsing specification
    it("should capture scala blocks with closures and ignore braces outside") {
      val txt = "This #{List(1, 2, 3).map{ \"Hi #\" + _ }} is an `#{example}` thing."
      knockoff( txt ).first match {
        case Paragraph( spans, _ ) =>
          spans.toList should equal( 
            List(
              Text("This "),
              InterpolatedSCAML("#{List(1, 2, 3).map{ \"Hi #\" + _ }}"),
              Text(" is an "),
              CodeSpan("#{example}"),
              Text(" thing.") ) )
        case _ => fail( "Unexpected result." )
      }
    }
    
    it("should capture an ending thing") {
      knockoff("A #{foo.bar}").first match {
        case Paragraph( spans, _ ) =>
          spans.toList should equal( List( Text("A "), InterpolatedSCAML("#{foo.bar}") ) )
        case b : Block => fail( "Unexpected block " + b )
      }
    }



## Writing ##

Generally speaking, SCAML is just going to be passed through without much fuss or
assistance of any kind (since we're assuming that the sequences will be dealt with
by a templating engine). We want to avoid any XML escapes, etc. So each of the
writers just needs to be adjusted to handle the `InterpolatedSCAML` type.

    // The SCAML writers
    trait SCAMLXHTMLWriter extends XHTMLWriter {
      
      override def spanToXHTML : Span => Node = span => {
        span match {
          case InterpolatedSCAML( content ) => Unparsed( content )
          case _ => super.spanToXHTML( span )
        }
      }
    }
    
    trait SCAMLLatexWriter extends LatexWriter {
      
      override def toLatex( span : Span )( implicit writer : Writer ) : Unit =
        span match {
          case InterpolatedSCAML( content ) => writer.write( content )
          case _ => super.toLatex( span )
        }
    }
    
    trait SCAMLPlainTextWriter extends TextWriter {
     
      override def spanToText( span : Span )( implicit writer : Writer ) : Unit = {
        span match {
          case InterpolatedSCAML( content ) => writer.write( content + " " )
          case _ => super.spanToText( span )
        }
      }
    }
    
    // In com/tristanhunt/knockoff/extra/SCAML.scala
    package com.tristanhunt.knockoff.extra
    
    import com.tristanhunt.knockoff._
    import com.tristanhunt.knockoff.latex.{ LatexWriter }
    import java.io.Writer
    import scala.xml.{ Node, Unparsed }
    
    // See the InterpolatedSCAML class
    
    // See the SCAMLDiscounter trait
    
    // See the SCAML writers
    
    
    // In test com/tristanhunt/knockoff/extra/SCAMLSpec.scala
    package com.tristanhunt.knockoff.extra
    
    import com.tristanhunt.knockoff._
    import org.scalatest._
    import org.scalatest.matchers._
    
    class SCAMLSpec extends Spec with ShouldMatchers {
      
      val discounter = DefaultWholesaler
      import discounter._
      
      describe( "SCAML Parser" ) {
        // See the SCAML specification
      }
    }

[Scalate]: http://scalate.fusesource.org/
[SCAML]: http://scalate.fusesource.org/documentation/scaml-reference.html