
#' Clustering coefficient of each layer in the multiplex network.
#'
#' @description The clustering coefficient measures the probability that the adjacent vertices of a vertex are connected.
#' @usage multiplex.transitivity(multiplex)
#' @param multiplex : The multiplex object.
#' @return A list of clustering coefficient of each layer
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' multiplex.transitivity(M)
#' @export

multiplex.transitivity <- function(multiplex){
            transitivity<- vector("integer")
            for(layer in multiplex@layers){
              transitivity<- c(transitivity,transitivity(graph = layer,type = "undirected",weights = NULL))
            }
            return(transitivity)
}
