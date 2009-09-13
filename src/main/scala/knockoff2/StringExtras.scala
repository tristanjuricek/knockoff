package knockoff2

trait StringExtras {
    
    class KnockoffString( val s : String ) {
     
        def substringOption( start : Int, finish : Int ) : Option[ String ] = {
            if ( start < finish )
                Some( s.substring( start, finish ) )
            else
                None
        }
     
        /**
         * Return the next N indices of a string where the sequence is found.
         * @return A list of size n if found, otherwise Nil
         */
        def nextNIndicesOf( n : Int, str : String ) : List[ Int ] = {
            val found = nextIndexOfN( n, str, s.length, Nil )
            if ( found.length == n ) found else Nil
        }

        /** Recursive implementation that builds up the list of indices. */
        private def nextIndexOfN(
                left    : Int,
                str     : String,
                index   : Int,
                current : List[ Int ]
            ) : List[ Int ] = {

            if ( left <= 0 || index < 0 ) return current
            
            val next = s.lastIndexOf( str, index - 1 )
            
            nextIndexOfN(
                left - 1,
                str,
                next,
                if ( next > 0 ) next :: current else current
            )
        }
    }
    
    implicit def KnockoffString( s : String ) = new KnockoffString( s )
}
