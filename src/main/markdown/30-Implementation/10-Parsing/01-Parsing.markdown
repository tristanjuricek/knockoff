Parsing Overview
================

Parsing is done in three steps:

1. Chunk Parsing - The document is converted to a series of Chunk objects, each
eventually mapping to a block. This is kicked off by the `ChunkStreamFactory`. A
`Chunk` is generally a "block-level" element, but the final determination of what
is a block level element isn't complete until step 3.

2. Span Parsing - The spans of each chunk are identified.

3. Object model creation.

Note that things like block quotes and more complex lists turn into "documents
within documents".



### A Bit Of History To Satisfy Myself ###

In my first attempt, I tried building one big parser combinator, and then, slowly,
some part of my brain fell down a well. What ended up happening, however, is that I
created an intermediate representation, but a wacky one, that was kind of hacked
together. It lacked consistency in how I represented whitespace. I ended up doing
lots of things, like:

* Preprocessing the text to switch tabs to spaces
* Injecting extra whitespace to make regular expression matching easier

Ug-ly.


