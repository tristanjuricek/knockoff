# The Knockoff Object Model #

First and foremost, the knockoff object model is intended for use only with Scala.

Documents are represented as a sequence of blocks. Each block can be a sequence of
spanning elements, or it can contain other blocks. The model is very similar to the
basic elements of HTML - headers, paragraphs, code, preformatted, blockquotes,
etc.



## Creating Blocks ##

Blocks should be relatively direct to create via factory methods. These methods will
sometimes allow you to create the child character sequences

    val header = heading( level: 1,
                          text("A ") :: strong("big") :: text(" deal") :: Nil )
    val block = paragraph( "Hey this is a paragraph with no children" )

When you have a sequence of blocks, you can get to it's related data primarily
through matching operations.

    block match {
      case Paragraph( content ) => //
      case Header( level, content ) => //
    }

Note that this *does not* mean that the following operation is valid:

    // Not guaranteed to work!
    block.asInstanceOf[ Paragraph ]

(This is a holdover from the original object design, which I've changed.)

### What can the block do?

The block object itself will likely contain information gathered during the parse.
But not all `Block` objects are necessarily created via a parser, since you can
create them with the factory methods mentioned earlier. Positional information may
be lacking.



## Block Types ##

First, there are the simple block types. These contain a character spanning 
sequences, usually called `content`.

* Plain Text
* Paragraph
* Header, which has a level from 1 to 6.
* Link Definition, a URL with an associated identifier.
* Code

The complex block types are all composed of block spanning sequences, referenced
as `children`.

* Blockquote 
* Numbered List
* Bullet List

The remaining block types all have special rules on how they are composed.

* Horizontal Rule
* Metadata - a key, value map associated with the document.
* "Raw" HTML
* Table

### `Blocks` - The parsing result type

The parsing results maintain state during the combinator parsing. Anything found
should be easy to reference within the document. This enables applications to
reference data from the source, rather than simply knowing how to convert it.

    // The Block definition
    trait Block {
      def source : String
      def pos : Position = NoPosition
    }


### `LinkDefinition`

This is the only metadata actually identified within a Markdown document officially.

__TODO__ Does this class have value as part of the public API?

__TODO__ I wonder if LinkDefinition is just a special case of document metadata,
which contains a string ID, some value definition, then a position (with length or
source). Extraction could be used to turn the data into something special.

    // The LinkDefinition definition
    case class LinkDefinition( val uri : URI, val id : String,
                               val title : Option[String], val source : String,
                               override val pos : Position )
    extends    Block

### `EmptyBlock`
  
    // The EmptyBlock definition
    case class EmptyBlock( val source : String, override val pos : Position )
    extends    Block
    
### `BlockEnd`

    // The BlockEnd definition
    case class BlockEnd( val source : String, override val pos : Position )
    extends    Block

### `PlainText`

    // The PlainText definition
    case class PlainText( val text : String, val source : String,
                          override val pos : Position )
    extends    Block



## Character Types ##

* Text
* Emphasis
* Strong
* Strikeout
* Superscript   
* Subscript     
* Small Caps     
* Quoted
* Citation
* Code           
* Space                 
* Em dash                
* En dash                
* Apostrophe            
* Ellipses              
* Line Break             
* Math
* TeX
* Inline Html
* Link
* Image 



## Object Model ##

    // In com/tristanhunt/knockoff2/ObjectModel.scala
    package com.tristanhunt.knockoff2
    
    import java.net.{ URI }
    import scala.util.parsing.input.{ NoPosition, Position }
    
    // See the Block definition
    
    // See the BlockEnd definition
    
    // See the EmptyBlock definition
    
    // See the LinkDefinition definition
    
    // See the PlainText definition