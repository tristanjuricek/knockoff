package knockoff2

trait HasElementFactory {

    def elementFactory : ElementFactory = defaultElementFactory
    
    private val defaultElementFactory = new ElementFactory
}
