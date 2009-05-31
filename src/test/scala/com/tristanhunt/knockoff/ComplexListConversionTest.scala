package com.tristanhunt.knockoff

import org.testng.annotations._
import org.testng.Assert._

@Test
class ComplexListConversionTest extends MkBlockParserFactory {
 
    def complexListConversionDoNothingTest {
     
        val list = List(
            MkParagraph( "p1" ),
            MkParagraph( "p2" ),
            MkParagraph( "p3" )
        )
        
        val actual = mkBlockParser.convertSparseComplexLists( list, Nil )
         
        assertEquals( actual, list )
    }
    
    
    def splitTightComplexBulletList {
        
        val preliminary = List(
            BulletListMkBlock( List( "foo", "boo\n    * sub-item 1\n      more stuff\n    * sub-item 2\n", "goo" ) )
        )
        
        val expected = List(
            ComplexBulletListMkBlock( List(
                List( MkParagraph( "foo" ) ),
                List(
                    MkParagraph("boo\n"),
                    BulletListMkBlock( List( "sub-item 1\n  more stuff\n", "sub-item 2\n" ) )
                ),
                List( MkParagraph( "goo" ) )
            ) )
        )
        
        val actual = mkBlockParser.convertTightComplexLists( preliminary, Nil )
        
        assertEquals( actual, expected )
    }
    
    
    def splitTightComplexNumberedList {
        
        val preliminary = List(
            NumberedListMkBlock( List(
                "foo",
                "boo\n    1. sub-item 1\n      more stuff\n    2. sub-item 2\n",
                "goo"
            ) )
        )
        
        val expected = List(
            ComplexNumberedListMkBlock( List(
                List( MkParagraph( "foo" ) ),
                List(
                    MkParagraph("boo\n"),
                    NumberedListMkBlock( List( "sub-item 1\n  more stuff\n", "sub-item 2\n" ) )
                ),
                List( MkParagraph( "goo" ) )
            ) )
        )
        
        val actual = mkBlockParser.convertTightComplexLists( preliminary, Nil )
        
        assertEquals( actual, expected )
    }
    
 
    def combineSparseComplexBulletList {
        
        val preliminary = List(
            BulletListMkBlock( List( "foo", "boo" ) ),
            CodeMkBlock( "    * item\n    with lines\n     * foo\n" ),
            BulletListMkBlock( List( "goo" ) )
        )
        
        val expected = List(
            ComplexBulletListMkBlock( List(
                List( MkParagraph( "foo" ) ),
                List(
                    MkParagraph( "boo" ),
                    BulletListMkBlock( List( "item\nwith lines\n", "foo\n" ) )
                )
            ) ),
            BulletListMkBlock( List( "goo" ) )
        )
        
        val actual = mkBlockParser.convertSparseComplexLists( preliminary, Nil )
        
        assertEquals( actual, expected )
    }
    
    
    def combineSparseComplexNumberedList {
        
        val preliminary = List(
            NumberedListMkBlock( List( "boo" ) ),
            CodeMkBlock( "    * item\n    with lines\n     * foo\n" )
        )
        
        val expected = List(
            ComplexNumberedListMkBlock( List( List(
                    MkParagraph( "boo" ),
                    BulletListMkBlock( List( "item\nwith lines\n", "foo\n" ) ),
            ) ) )
        )
        
        val actual = mkBlockParser.convertSparseComplexLists( preliminary, Nil )
        
        assertEquals( actual, expected )
    }
}