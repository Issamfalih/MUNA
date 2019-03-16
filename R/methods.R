

                                      #This R file contains the methods of the multiplex class





#' Get the layer's  name attribute of the multiplex network
#'
#' @description get the layer's  name attribute of the multiplex network
#' @param multiplex : The multiplex object.
#' @param id_layer : The id of the layer in the multiplex network.
#' @return String, the layer's  name attribute.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' print(get_layer_name.multiplex(M,3))
#' @export
get_layer_name.multiplex <- function(multiplex, id_layer){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  # this function returns the name of layer by his number
  net <- get_layer_by_number.multiplex(multiplex,id_layer)
  if(!is.null(net))
    return(igraph::get.graph.attribute(net,name="name"))
  else
    stop(paste("Unknown layer number ", id_layer, sep=""))
}








#' Set the layer's name attribute of the multiplex network
#'
#' @description set the layer's name attribute of the multiplex network
#' @param multiplex : The multiplex object.
#' @param id_layer : The id of the layer in the multiplex network.
#' @param name : The new name of the layer.
#' @return The multiplex network, with the layer's name attribute added or set.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' M <- set_layer_name.multiplex(M, 2, "Co-work layer")
#' @export
set_layer_name.multiplex <- function(multiplex, id_layer, name){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(!is.null(get_layer_by_number.multiplex(multiplex,id_layer))||!is.character(name)){
    multiplex@layers[[id_layer]] <- igraph::set.graph.attribute(multiplex@layers[[id_layer]],"name",value = name)
    return(multiplex)
  }
  else
    stop(paste("Unknown layer number ", id_layer, sep=""))
}





#' Delete a layer in the multiplex network
#'
#' @description Delete a layer in the multiplex network
#' @param id_layer : The id of the layer to delete.
#' @param multiplex : The multiplex object.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @return The multiplex network, with the layer removed
#' @examples
#' M <- Multiplex_Lazega
#' M <- delete_layer_by_number.multiplex(M,2)
#' @export
delete_layer_by_number.multiplex <- function(multiplex, id_layer){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")

  print(get_number_of_layers.multiplex(multiplex))
  print(id_layer)
  if((id_layer>0)&&(id_layer<=get_number_of_layers.multiplex(multiplex))){
    multiplex@layers[[id_layer]] <- NULL
    return(multiplex)
  }
  else
    stop(paste("there is no layer number ",id_layer,sep=""))
}





#' Delete a layer in the multiplex network
#'
#' @description Delete a layer in the multiplex network
#' @param name : The name of the layer to delete.
#' @param multiplex : The multiplex object.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @return The multiplex network, with the layer removed
#' @examples
#' M <- Multiplex_Lazega
#' M <- delete_layer_by_name.multiplex(M,"Advice")
#' @export
delete_layer_by_name.multiplex <-  function(multiplex, name){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(get_number_of_layers.multiplex(multiplex)==0)
    return(multiplex)
  for(i in seq(get_number_of_layers.multiplex(multiplex))){
    print(i)
    if(igraph::get.graph.attribute(multiplex@layers[[i]],name="name")==name){
      multiplex@layers[[i]] <- NULL
      multiplex <- delete_layer_by_name.multiplex(multiplex,name)
      break
    }
  }
  return(multiplex)
}




#' Get a layer in the multiplex network
#'
#' @description get a layer in the multiplex network
#' @param name : The name of the layer to be return.
#' @param multiplex : The multiplex object.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @return The igraph graph representing the layer with the name given in parameter, Null if nothing
#' @examples
#' library(igraph)
#' M <- Multiplex_Lazega
#' g <- get_layer_by_name.multiplex(M, name="Co-work")
#' str(g)
#' plot(g)
#' @export
get_layer_by_name.multiplex <- function(multiplex, name){
  # this function return the igraph graph representing layer's name:  layer_name and return NULL if nothing
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(length(multiplex@layers)==0)
    return(NULL)
  for(layer in multiplex@layers){
    if(igraph::get.graph.attribute(layer,"name")==name)
      return(layer)
  }
  return(NULL)
}





#' Get the attribute name of the multiplex network
#'
#' @description get the attribute name of the multiplex network
#' @param multiplex : The multiplex object.
#' @return String, the name of the multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' print(get_name.multiplex(M))
#' @export
get_name.multiplex <- function(multiplex){
  # this method returns the name of the multiplex graph
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  return(multiplex@name)
}




#' Set the attribute name of the multiplex network
#'
#' @description set the attribute name of the multiplex network
#' @param multiplex : The multiplex object.
#' @param name : The name of the multiplex network.
#' @return The multiplex network, with the name attribute added or set.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' M <- set_name.multiplex(M, "Lazega Multiplex Network")
#' print(get_name.multiplex(M))
#' @export
set_name.multiplex <-  function(multiplex,name){
  # By default the name is Multiplex
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(name)&&(name!="")){
    multiplex@name = name
    return(multiplex)
  }
  else  stop("Invalid name")
}




#' Get the number of layers in the multiplex network
#'
#' @description Get the number of layers in the multiplex network
#' @param multiplex : The multiplex object.
#' @return Numeric scalar, an integer representing the number of layers in the multiplex network.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' get_number_of_layers.multiplex(M)
#' @export
get_number_of_layers.multiplex <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  return(length(multiplex@layers))
}






#' Get the multiplex layer which has an id number given in parameters
#'
#' @description Get the multiplex layer which has an id number given in parameters
#' @param layer_number : Numeric scalar, an integer representing the id number of a layer
#' @param multiplex : The multiplex object.
#' @return An igraph graph representing layer widh id layer_number, NULL if ths id layer_number don't exist
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' g <- get_layer_by_number.multiplex(M,2)
#' plot(g)
#' @export
get_layer_by_number.multiplex <- function(multiplex, layer_number){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if((layer_number>0)&&(layer_number<=length(multiplex@layers)))
    return(multiplex@layers[[layer_number]])
  else
    return(NULL)
}




#' Is this object a multiplex network ?
#'
#' @description is this object a multiplex network ?
#' @param multiplex : An R object
#' @return A logical constant, TRUE if argument is a multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' is.multiplex(M)
#' is.multiplex(numeric(15))
#' @export
is.multiplex <- function(multiplex){

  if(!.hasSlot(multiplex,"layers")||!.hasSlot(multiplex,"nodes")||!.hasSlot(multiplex,"name"))
    return(FALSE)

  layers <- multiplex@layers
  nodes <- multiplex@nodes
  if(is.null(layers)&&is.null(nodes))
    return(TRUE)
  if((!is.null(layers)&&is.null(nodes))||(is.null(layers)&&!is.null(nodes)))
    return(FALSE)

  for(layer in layers){
    if(!igraph::is.igraph(layer)||(igraph::vcount(layer)!=length(nodes)))
        return(FALSE)
    }
  return(TRUE)
}

