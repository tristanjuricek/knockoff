# Part 1. Welcome to Knockoff #

Knockoff is a Markdown parser with a twist. Most Markdown systems just convert
Markdown source to HTML. Knockoff converts from Markdown source to an object model
... then, to an XHMTL fragment, generated easily with Scala's excellent XML
integration. I've found it an easy way to manipulate Markdown content.

For example, I've built a [literate programming][literable] environment by basically
stringing together specially tagged code blocks of the markdown content. With the
meta-data extension, you can pretty easily use markdown files as the "storage"
format for something like a website.



## Converting Markdown to HTML ##

The `Discounter` is what grabs the "knockoff" of a markdown string. For simple usage,
you can use the `DefaultDiscounter` object.

    import com.tristanhunt.knockoff.DefaultDiscounter._
    toXHTML( knockoff( markdownString ) )

See the [Recipes](2.%20Usage/2.Recipes.html) page for more.



## Getting Started ##

### Using Knockoff With [SBT][]

Right now knockoff is easiest to use via [sbt][]. The dependency declaration that
resolves the scala version looks like this:

    val knockoff = "com.tristanhunt" % "knockoff" %% "0.7.0-10"

If you have a problem with the scala version, you can specify it directly as part
of the project name:

    val knockoff = "com.tristanhunt" % "knockoff_2.7.7" % "0.7.0-10"

The repository specification is:

    val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"

A very simple example of the project definition file that goes in a place like
`project/build/MyProject.scala`:

    import sbt._
    
    class MyProject( info : ProjectInfo ) extends DefaultProject( info ) {
      val knockoff  = "com.tristanhunt" % "knockoff_2.7.7" % "0.7.0-10"
      val nexus =
        "tristanhunt nexus" at "http://tristanhunt.com:8081/content/groups/public"
    }

### Using Knockoff With Maven

An example dependency definitions includes the scala version as part of the
`artifactId`.

    <dependency>
      <groupId>com.tristanhunt</groupId>
      <artifactId>knockoff_2.7.7</artifactId>
      <version>0.7.0-10</version>
    </dependency>

You'll likely need to grab things off of my nexus host, which can be done with this
repository definition:

    <repository>
      <id>tristanhunt.com</id>
      <name>Tristan's Repo</name>
      <url>http://tristanhunt.com:8081/content/groups/public/</url>
    </repository>

### Using Knockoff Elsewheres

Definitely possible, it will require a downloaded [scala][] version, after which you
include the `lib/scala-library.jar` file in your classpath along with the file at

    http://tristanhunt.com:8081/content/groups/public/com/tristanhunt/
      knockoff_[SCALA_VERSION]/
      [KNOCKOFF_VERSION]/
      knockoff_[SCALA_VERSION]-[KNOCKOFF_VERSION].jar



## Building knockoff source ##

It uses [sbt][]:

    sbt update package

It also uses [literable][], which is what generates both the source code and
documentation. Add the `lit` sub command.

    sbt update lit package





## Recipes ##

To run the code, import the `DefaultDiscounter`:

    import com.tristanhunt.knockoff.DefaultDiscounter._

### Get the Block Sequence

    val blocks = knockoff( markdownString )

### ... Make an XHTML Fragment

    val xhtml = toXHTML( blocks )
    val sw = new StringWriter
    XML.write( sw, xhtml, "utf-8", false, null )
    val myString = sw.toString

### Grab The Title Of The First Header

    blocks.find( _.isInstanceOf[Header] ).map( toText ).getOrElse( "No header" )

### Create an HTML Page 

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

### Prettify the Code Blocks 

An example of how to tweak the object model.

First, extend the `CodeBlock` to write the extra `class` attribute.

    class   PrettifiedCodeBlock( text : Text, pos : Position )
    extends CodeBlock( text, pos ) {

      override def toXML : Node =
        <pre><code class="prettyprint">{ preformatted }</code></pre>
    }

Then setup the factory to create your `PrettifiedCodeBlock` instead.
    
    trait PrettyDiscounter extends Discounter {
      override def elementFactory = new ElementFactory {
        override def codeBlock( t : Text, p : Position ) : CodeBlock =
          new PrettifiedCodeBlock( t, p )
      }
    }

You'll have to create your own `Discounter` type now, however:

    object MyDiscounter extends PrettyDiscounter
    import MyDiscounter._

### Print Up A Page Contents Map

First you'll need to convert the XHTML output and generate anchor links around
all the headings in the page.

    lazy val discounter : Discounter = new Discounter {

      override def headerToXHTML : ( Int, Seq[Span] ) => Node = (level, spans) => {
        val spanned = spans.map( spanToXHTML(_) )
        val name = toAnchorName( headerText(spans) )
        // Map the level and make sure the HTML has the name attribute.
        level match {
          case 1 => <h1><a name={name}>{ spanned }</a></h1>
          case 2 => <h2><a name={name}>{ spanned }</a></h2>
          case 3 => <h3><a name={name}>{ spanned }</a></h3>
          case 4 => <h4><a name={name}>{ spanned }</a></h4>
          case 5 => <h5><a name={name}>{ spanned }</a></h5>
          case 6 => <h6><a name={name}>{ spanned }</a></h6>
          case _ => <div class={ "header" + level }><a name={name}>{ spanned }</a></div>
        }
      }
    }

You'll need to reuse these methods later:

    def headerText( spans : Seq[Span] ) : String = {
      val stringWriter = new java.io.StringWriter
      spans.map( wholesaler.spanToText(_)(stringWriter) )
      return stringWriter.toString        
    }
    
    def toAnchorName( str : String ) : String = {
      "a-" + str.trim.toLowerCase
                .replaceAll("\\s+"," ")
                .replaceAll("[^a-z0-9._\\-:]","_")
    }

Here, I'll create an HTML list based on the headers of the document. This will not
include any headers smaller than level 3. It also replaces any extra styling within
the header, since I don't want those in the table of contents map.

    def makePageTOC( blocks : Seq[Block] ) : String = {
      val headers =
        for ( header <- blocks.filter(_.isInstanceOf[Header]).map(_.asInstanceOf[Header])
              if ( header.level < 4 ) )
          yield header
      // If there are no headings, well, ignore this.
      if ( headers.length == 0 ) return ""
      val xhtml =
        <ul id="pageTOC">{
          for { header <- headers
                text = headerText( header.spans )
                name = "#" + toAnchorName( text ) }
            yield <li><a href={name}>{ text }</a></li>
        }</ul>
      val sw = new java.io.StringWriter
      XML.write( sw, xhtml, "utf-8", false, null )
      return sw.toString
    }



## Where Knockoff Is Not Like Markdown ##

Knockoff does not generate HTML 100% exactly like the core Markdown script.

1. Tabs are passed through. Why you're using tabs is beyond me, this will keep
tools like `diff` honest and functional.

2. List items (`<li>`) only have a sub-paragraph (`<p>`) if you have complex
content. (In the core script, if you space them widely, you get the sub `<p>`
element, which made no sense to me.)

3. If you have a code line, followed by another indented line, even if that line's
empty, the empty line is part of the code block.

Other than this, Knockoff passes the markdown test suite. I've even extended it with
my own examples, which I use to pass most of my own tests.



## Recent Updates ##

#### `0.7.3-15` August 21, 2010

* Fix where CRLF sequences would break the empty space matcher.

* First pass at a markdown test data generator. (2.8.0 only, requires tweaking the build file.)

#### `0.7.3-14` July 14, 2010

* Added escapes for a few known special characters.

* Fixed inline LaTeX parsing (issue [#24](http://github.com/tristanjuricek/knockoff/issues/closed#issue/24)).

* Added the SnuggleTeX dependency to spit out MathML in the Wholesaler for
  previously mentioned inline TeX. I note that MathML is kind of, well, poorly
  supported, so YMMV.

* Big revision of the literate markdown.

* Fixed problems when combining MetaData with the SCAML writer.

#### `0.7.2-13` June 7, 2010

* Added the `SCAMLWriter` with associated `toSCAML` method, in case you want to
  pepper your markdown files with SCAML - and then use [Scalate](http://scalate.fusesource.org)
  to process them. (_Note_ No SSP... yet.)

* Specification bugfix whereupon hard breaks are properly broken with `<br/>`s
  instead of splitting the two lines into separate paragraphs.

#### `0.7.1-12` June 1, 2010

Two small bug fixes:

1. A reference link followed by a paren was being matched as a normal link.

1. If you used an asterix-delimited em right after a list item asterix marker, the
   line is now considered to be a list item if you use an odd number of asterixes
   on the line. (Ugly, but generally going to be OK.)


#### `0.7.1-11` May 17, 2010

* Added SCAML pass-through to the Wholesaler.

* Added special case where a leading emphasis or strong element would cause the
  paragraph to be interpreted as a text string.


#### `0.7.0-10` March 22, 2010

This is a major revision of the project. Your code will probably break if you did
something other than just convert simple markdown to XHTML.

Generally speaking, the project is taking a few ideas from [pandoc][], starting
with output to LaTeX.

* Redid the entire object model to remove the `xml` and `markdown` methods, and the
  silly little object factory. If you need to customize the output, override one
  of the `FooWriter` traits.

* Added plain text and a *preliminary* LaTeX writer.

* Fixed an issue where two spaces at the end of the line were not breaking into
  separate paragraphs.
  
* Removed the use of perl for the integration tests. Everything should be kicked
  off by the `test` command.
  
* Cross-compiled for Scala 2.7.4-2.7.7. The core project did run under 2.8.0
  recently, but getting the sbt configuration straight for ScalaTest isn't as "plug
  and play" as I would have liked.

#### `0.6.1-9` March 7, 2009

A Java 1.5 build of 0.6.1.

#### `0.6.1-8` Dec 6, 2009

* Changed the output from `GroupSeq` to `BlockSeq` of `Wholesaler`.
* Created the `MetaData` extension, which allows for extra document-related info.
* Added `Wholesaler` class to `com.tristanhunt.knockoff.extra` namespace, which
  is the container for Markdown extensions.

#### `0.6.0-5` Nov 28, 2009

* Small alteration to the `CodeBlock` that doesn't force a newline at the end of
  every code block. I have no idea what I was thinking here.

#### `0.6.0-4` Nov 28, 2009

* Set package name as `com.tristanhunt.knockoff`. [Let's try being standard!]
* Fixed whitespace problem that caused crash, and trying to report failures better.
* Added `Nothin But Code` test to suite, which triggered a crash during parsing.
* Upgraded project to literable `0.5.0-3`, which involves moving `src/main/markdown`
  to `literate`.


[scala]: http://scala-lang.org
[literable]: http://tristanhunt.com/projects/literable
[pandoc]: http://johnmacfarlane.net/pandoc/
[literable]: http://tristanhunt.com/projects/literable
[sbt]: http://code.google.com/p/simple-build-tool/
[scala]: http://www.scala-lang.org