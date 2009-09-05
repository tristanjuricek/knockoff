Recipes
=======

### Grab The First Header ###

    blocks.filter( _.isInstanceOf[ Header ] ).firstOption match {
        case Some( header ) => 
        case None => error( "No header" )
    }
