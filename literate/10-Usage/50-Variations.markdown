# Where Knockoff is not like Markdown #

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
