Introduction to Knockoff
========================

Knockoff is a Markdown parser with a twist. Most Markdown systems just convert
Markdown source to HTML. Knockoff converts from Markdown source to an
in-memory model ... with a handy (X)HTML version. It's written in [Scala][1],
whose power affords us simple and fast extension and generation.

Simply put, it was rather easy for me to build a [literate programming][2]
environment on top of an object model. This is more than just "generating" web
documents. It's the backbone of a great "only what you need" approach to doing
things built for web distribution.

[1]: http://scala-lang.org
[2]: http://tristanhunt.com/projects/literable


### New feature: Allow The Position Of The Model To Come Through

Say we have the document with line numbers:

    1: Header 1
    2: ========
    3:
    4: Paragraph one
    5:
    6:     Code block 1
    7:
    8: Paragraph two

It would be _very_ handy to have the line numbers. This allows us to backwards
patch into the original markdown document.

    scala> blockSeq(2).position.startLine
    Int = 6
    
    scala> blockSeq(0).position.endLine
    Int = 2

Note that this is only significant for `Block` elements. And in reality I'll
probably start with 0.