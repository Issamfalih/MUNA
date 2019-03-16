

#' Adjacency matrix of the multiplex network
#'
#' @description Convert the multiplex network to a list of adjacency matrix of each layer.
#' @usage get_adjacency.multiplex(multiplex)
#' @param multiplex : The multiplex network to convert.
#' @return the list of adjacency matrix of each layer in the multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' print(get_adjacency.multiplex(M))
#' @export

get_adjacency.multiplex <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  nbLayers = get_number_of_layers.multiplex(multiplex)
  vect <- vector("list",nbLayers)
  for(l in seq(nbLayers)){
    vect[[l]] <- get.adjacency(multiplex@layers[[l]])
  }
  return(vect)

}
