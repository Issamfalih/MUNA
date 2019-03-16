
#' The maximal connected components of each layer in the multiplex network
#'
#' @description Calculate the maximal  connected components of each layer in the multiplex network
#' @usage components.multiplex(multiplex)
#' @param multiplex : The multiplex object to analyse.
#' @return A list of the maximal connected components of each layer in the multiplex network. Each components contain a named list with three components:
#' @return  membership numeric vector giving the cluster id to which each vertex belongs.
#' @return  csize numeric vector giving the sizes of the clusters.
#' @return no  numeric constant, the number of clusters.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' components.multiplex(M)
#' @export
components.multiplex <- function(multiplex){
  # thsi function return a list of the maximal connected components of each layer in the multiplex graph
            clust<- list()
            for(i in seq(get_number_of_layers.multiplex(multiplex))){
              #clust <- clusters(i)$csize
              graph <- get_layer_by_number.multiplex(multiplex,i)
              clust[[i]] <- components(graph)
            }

  return(clust)
}
