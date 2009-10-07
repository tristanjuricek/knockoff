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


## Variations off of Markdown ##

This has a couple of _very_ subtle adjustments to the base Markdown script:

1. Tabs are passed through. Though why you're using tabs is beyond me, this keeps
like diff tools honest.

2. List items (`<li>`) only have a sub-paragraph (`<p>`) if you have complex
content. (In the core script, if you space them widely, you get the sub `<p>`
element, which made no sense to me.)

3. If you have a code line, followed by another indented line, even if that line's
empty, the empty line is part of the code block.


[1]: http://scala-lang.org
[2]: http://tristanhunt.com/projects/literable
