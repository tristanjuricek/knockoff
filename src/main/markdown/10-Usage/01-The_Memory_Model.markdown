The In-Memory Model
===================

In the end, a Markdown document is a list of block elements, each of which is
a list of spanning elements. You could say this is a CSS-oriented view of the
page.

Each of these elements makes the output HTML in XHTML format:

    blocks.xml

So can the spans:

    spans.xml

Some blocks can contain other blocks:

    blockquote.children

And some spanning elements, too:

    emphasis.children

### Finding Specific Things

You dont

