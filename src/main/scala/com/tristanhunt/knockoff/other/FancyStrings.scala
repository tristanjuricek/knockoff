package com.tristanhunt.knockoff.other

object FancyStrings {
 
    def caseInsensitiveOrder(key:String) = new Ordered[String] {
        def compare(toKey:String):Int = key compareToIgnoreCase toKey
    }
}