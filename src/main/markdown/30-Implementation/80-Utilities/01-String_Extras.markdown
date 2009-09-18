# `StringExtras` #

Adds utilities to String for doing things like finding the next N indexes of a
recurrence.

    // In knockoff2/StringExtras.scala
    package knockoff2
    
    import scala.collection.mutable.ListBuffer
    
    trait StringExtras {
        
        class KnockoffString( val wrapped : String ) {
         
            def substringOption( start : Int, finish : Int ) : Option[ String ] = {
                if ( start < finish )
                    Some( wrapped.substring( start, finish ) )
                else
                    None
            }
         
            /**
             * Return the next N indices of a string where the sequence is found.
             * @return A list of size n if found, otherwise Nil
             */
            def nextNIndicesOf( n : Int, str : String ) : List[ Int ] = {
                val found = nextIndexOfN( n, str, -1, new ListBuffer )
                if ( found.length == n ) found else Nil
            }

            /** Recursive implementation that builds up the list of indices. */
            private def nextIndexOfN(
                    left    : Int,
                    str     : String,
                    index   : Int,
                    current : ListBuffer[ Int ]
                ) : List[ Int ] = {

                if ( left <= 0 || index >= wrapped.length ) return current.toList
                
                val next = wrapped.indexOf( str, index )
                
                if ( next >= 0 ) current += next
                
                nextIndexOfN( left - 1, str, next + 1, current )
            }
        }
        
        implicit def KnockoffString( s : String ) = new KnockoffString( s )
    }

### `StringExtrasSpec`

    // In test knockoff2/StringExtrasSpec.scala
    package knockoff2
    
    import org.scalatest._
    import org.scalatest.matchers._
    
    class   StringExtrasSpec
    extends Spec
    with    ShouldMatchers
    with    ColoredLogger
    with    StringExtras {
        
        describe("StringExtras.nextNIndices") {

            it( "should find two different groups of the same time" ) {
                "a `foo` b `bar`".nextNIndicesOf(2,"`") should equal ( List( 2, 6 ) )
            }

            it( "should deal with only one index" ) {
                "a `foo with nothin'".nextNIndicesOf(2, "`") should equal (Nil)
            }
        }
    }