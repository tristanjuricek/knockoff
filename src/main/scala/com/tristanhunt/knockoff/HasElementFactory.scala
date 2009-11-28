package com.tristanhunt.knockoff

trait HasElementFactory {

    def elementFactory : ElementFactory = defaultElementFactory
    
    private val defaultElementFactory = new ElementFactory
}
