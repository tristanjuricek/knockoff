Introduction to Knockoff
========================

Knockoff is a Markdown parser with a twist. Most Markdown systems just convert
Markdown source to HTML. Knockoff converts from Markdown source to an
in-memory model ... with a handy (X)HTML version. It's written in [Scala][1],
whose power affords us simple and fast extension.

This is more than just "generating" web documents. It's the backbone of a great
"only what you need" approach to doing things built for web distribution.

For example, it was easy for me to build a [literate programming][2] environment on
top of an object model. This approach "tags" specific bits of markdown code blocks
with special meaning - stick this code block in a separate file in a particular
format. And then, the markdown document itself is placed into a website.


## Converting Markdown to HTML Knockoff ##

Anyhow, the main thing you do with knockoff is translate Markdown documents into
HTML. In Scala code, this is done via:

    DefaultDiscounter.knockoff( markdownString ).toXML

This returns the `Group` representation of the document.

This is pretty easy to call from the console as well, using a shell script set up
in the root of the project tree:

    ./discounter [file ...]


## What is This `Discounter` Thing, What The Hell Am I Thinking? ##

The `Discounter` is the base object you can use to start to specialize how the
document is parsed.


## Recent Updates ##

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
