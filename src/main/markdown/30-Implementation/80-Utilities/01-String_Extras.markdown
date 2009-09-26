# `StringExtras` #

Adds utilities to String for doing things like finding the next N indexes of a
recurrence.

    // In knockoff2/StringExtras.scala
    package knockoff2
    
    import scala.collection.mutable.ListBuffer
    
    trait StringExtras {
        
        class   KnockoffCharSequence( val seq : CharSequence )
        extends KnockoffString( seq.toString )
        
        class KnockoffString( val wrapped : String ) {
       
          def substringOption( start : Int, finish : Int ) : Option[ String ] = {
            if ( start < finish )
              Some( wrapped.substring( start, finish ) )
            else
              None
          }
          
          def toOption : Option[ String ] =
            if ( wrapped.isEmpty ) None else Some( wrapped )
       
          /**
           * Return the next N indices of a string where the sequence is found.
           * @return A list of size n if found, otherwise Nil
           */
          def nextNIndicesOf( n : Int, str : String ) : List[Int] = {
            val found = nextIndexOfN( n, str, -1, new ListBuffer )
            if ( found.length == n ) found else Nil
          }

          /**
            Recursive implementation that builds up the list of indices.
            @param left The number of indexes remaining to be found.
            @param str The source string.
            @param index Where we start our search.
            @param current The indexes we've found so far.
          */
          private def nextIndexOfN(
              left    : Int,
              str     : String,
              index   : Int,
              current : ListBuffer[Int]
            ) : List[Int] = {

            if ( left <= 0 || index >= wrapped.length ) return current.toList
            val next = wrapped.indexOf( str, index )
            if ( next >= 0 ) current += next
            nextIndexOfN( left - 1, str, next + 1, current )
          }
          
          /**
            Locates proper parenthetical sequences in a string.
          */
          def findBalanced(
              open  : Char,
              close : Char,
              start : Int
            ) : Option[Int] = {
          
            val nextOpen = wrapped.indexOf( open, start )
            if ( (nextOpen == -1) || (wrapped.length == nextOpen + 1) ) return None
            findBalancedClose( 1, open, close, start + 1 )
          }
          
          /**
            Recursive method for paren matching that is initialized by findBalanced.
          */
          private def findBalancedClose(
              count : Int,
              open  : Char,
              close : Char,
              index : Int
            ) : Option[Int] = {
              
            if ( wrapped.length >= index ) return None
           
            val nextOpen  = wrapped.indexOf( open, index )
            val nextClose = wrapped.indexOf( close, index )
            
            if ( nextClose == -1 ) return None
            
            // We find another unbalanced open
            if ( (nextOpen != - 1) && (nextOpen < nextClose) )
              return findBalancedClose( count + 1, open, close, index + 1 )
            
            // We have a balanced close, but not everything is done
            if ( count > 1 )
              return findBalancedClose( count - 1, open, close, index + 1 )
  
            // Everything is balanced
            Some( nextClose )
          }
        }

        implicit def KnockoffCharSequence( s : CharSequence ) =
          new KnockoffCharSequence( s )
        
        implicit def KnockoffString( s : String ) = new KnockoffString( s )
    }

### `StringExtrasSpec`

    // In test knockoff2/StringExtrasSpec.scala
    package knockoff2
    
    import org.scalatest._
    import org.scalatest.matchers._
    
    class StringExtrasSpec extends Spec with ShouldMatchers with ColoredLogger
      with StringExtras {
        
      describe("StringExtras.nextNIndices") {

        it( "should find two different groups of the same time" ) {
          "a `foo` b `bar`".nextNIndicesOf(2,"`") should equal ( List( 2, 6 ) )
        }

        it( "should deal with only one index" ) {
          "a `foo with nothin'".nextNIndicesOf(2, "`") should equal (Nil)
        }
      }
    }