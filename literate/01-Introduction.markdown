Welcome to Knockoff
===================

Knockoff is a Markdown parser with a twist. Most Markdown systems just convert
Markdown source to HTML. Knockoff converts from Markdown source to an object model
... then, to an XHMTL fragment, generated easily with Scala's excellent XML
integration. I've found it an easy way to manipulate Markdown content.

For example, I've built a [literate programming][2] environment by basically
stringing together specially tagged code blocks of the markdown content. With the
meta-data extension, you can pretty easily use markdown files as the "storage"
format for something like a website.


## Converting Markdown to HTML ##

The `Discounter` is what grabs the "knockoff" of a markdown string. For simple usage,
you can use the `DefaultDiscounter` object.

    import com.tristanhunt.knockoff.DefaultDiscounter._
    toXML( knockoff( markdownString ) )

See the [Recipes](10-Usage/02-Recipes.html) page for more.


## Recent Updates ##

### `0.6.1-8` Dec 6, 2009

* Changed the output from `GroupSeq` to `BlockSeq` of `Wholesaler`.
* Created the `MetaData` extension, which allows for extra document-related info.
* Added `Wholesaler` class to `com.tristanhunt.knockoff.extra` namespace, which
  is the container for Markdown extensions.


### `0.6.0-5` Nov 28, 2009

* Small alteration to the `CodeBlock` that doesn't force a newline at the end of
  every code block. I have no idea what I was thinking here.

### `0.6.0-4` Nov 28, 2009

* Set package name as `com.tristanhunt.knockoff`. [Let's try being standard!]
* Fixed whitespace problem that caused crash, and trying to report failures better.
* Added `Nothin But Code` test to suite, which triggered a crash during parsing.
* Upgraded project to literable `0.5.0-3`, which involves moving `src/main/markdown`
  to `literate`.


[1]: http://scala-lang.org
[2]: http://tristanhunt.com/projects/literable
