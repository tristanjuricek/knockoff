package com.tristanhunt.knockoff


/**
 * This is probably the best way to include KnockOff into your project.
 */
object Imports extends ConverterHelpers {
    
    def knockoff( markdownString : String ) : KnockOff.KnockOffResult =
        KnockOff.parse( markdownString )
}