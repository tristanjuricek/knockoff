package com.tristanhunt.knockoff


trait ConverterHelpers {

    // Add toXML methods to blocks, lists of Nads (which are typically owned by blocks), and of
    // course, the block list itself.

    import scala.xml.{ Node, NodeBuffer }
    
    class BlockListConverter( blocks : List[ Block ] ) {
     
        def toXML : Seq[ Node ] = BlockConverter.toXML( blocks )
    }
    
    implicit def BlockListConverter( blocks : List[ Block ] ) : BlockListConverter =
        new BlockListConverter( blocks )

    class XMLBlock ( block : Block ) {
        
        def toXML : Node = BlockConverter.toXML( block )
    }
    
    implicit def XMLBlock( block : Block ) : XMLBlock = new XMLBlock( block )

    class XMLNadList ( nads : List[ Nad ] ) {
        
        def toXML : NodeBuffer = SpanConverter.span( nads )
    }
    
    implicit def XMLNadList( nads : List[ Nad ] ) : XMLNadList = new XMLNadList( nads )
}