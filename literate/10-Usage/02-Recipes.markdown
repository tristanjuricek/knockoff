Recipes
=======

### Just getting the block sequence ###

    import com.tristanhunt.knockoff._

    val blocks = DefaultDiscounter.knockoff( markdownString )


### ... And then make it an XHTML fragment ###

    blocks.toXML.toString


### Grab The First Header ###

    blocks.filterType( Headers ).firstOption {
      case Some( header ) => header.text
      case None => "no header!"
    }


### Create an HTML Page ###

Just a little inspiration of what you can do with Scala:

    import com.tristanhunt.knockoff._

    class Page( val markdown : String, val discounter : Discounter ) {

      def html = "<!doctype html>\n" + <html>{ head }{ body }</html>
      def head = <head><title>{ titleContent }</title></head>
      def body = <body>{ blocks.toXML }</body>
      
      lazy val blocks = discounter.knockoff( markdown )
      
      def titleContent =
        filterType( Headers ).firstOption {
          case Some( header ) => header.text
          case None => ""
        }
    }


### Tweak The HTML Output For Something Like Prettify ###

First, extend the `CodeBlock` to write the extra `class` attribute.

    class   PrettifiedCodeBlock( text : Text, pos : Position )
    extends CodeBlock( text, pos ) {

      override def xml : Node =
        <pre><code class="prettyprint">{ preformatted }</code></pre>
    }

Then setup the factory to create your `PrettifiedCodeBlock` instead.
    
    trait PrettyDiscounter extends Discounter {
      override def elementFactory = new ElementFactory {
        override def codeBlock( t : Text, p : Position ) : CodeBlock =
          new PrettifiedCodeBlock( t, p )
      }
    }

