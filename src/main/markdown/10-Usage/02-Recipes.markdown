Recipes
=======

### Just getting the block sequence ###

    import knockoff._
    val blocks = DefaultDiscounter.knockoff( markdownString )

### Make An HTML String ##

    blocks.toXML.toString

### Grab The First Header ###

    ( blocks ? Headers ).firstOption {
      case Some( header ) => header.text
      case None => "no header!"
    }

### Tweak The HTML Output For Something Like Prettify ###


    
    trait PrettyDiscounter extends {
      
    }