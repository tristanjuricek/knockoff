Recipes
=======

### Just getting the block sequence ###

    import knockoff._
    val blocks = DefaultDiscounter.knockoff( markdownString )

### Make An HTML String ##

    blocks.toXML.toString

### Grab The First Header ###

    ( blocks ? Headers ).firstOption {
      case Some( header ) => header.text
      case None => "no header!"
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