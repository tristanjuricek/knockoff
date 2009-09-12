Parsing Overview
================

Parsing is done in three steps:

1. Chunk Parsing - The document is converted to a series of Chunk objects, each
eventually mapping to a block. This is kicked off by the `ChunkStreamFactory`.

2. Span Parsing - The spans of each chunk are identified.

3. Object model creation.

Note that things like block quotes and more complex lists turn into "documents
within documents".



1. "Lexical" recognition, where whitespace matters a lot. This is where we figure
out the block boundaries.

2. Span recognition, where whitespace only matters in a couple of cases. This is
probably where the final `Span` elements are located.

3. Object model combining, where we take the `Span`s and the extra lexical data and
figure out the final `Block` elements.


### A Bit Of History To Satisfy Myself ###

In my first attempt, I tried building one big parser combinator, and then, slowly,
some part of my brain fell down a well. What ended up happening, however, is that
I created an intermediate representation, but a wacky one, that was kind of hacked
together. It lacked consistency in how I represented whitespace. I ended up doing
lots of things, like:

* Preprocessing the text to switch tabs to spaces
* Injecting extra whitespace to make regular expression matching easier

Ug-ly.


