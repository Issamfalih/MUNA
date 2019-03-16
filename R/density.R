#'   Graph density of each layer in the multiplex network.
#'
#' @description The density of a graph is the ratio of the number of edges and the number of possible edges.
#' @usage multiplex.density(multiplex)
#' @param multiplex : The multiplex object.
#' @return A list of real constant. This function returns NaN(=0.0/0.0) for an empty multiplex with zero vertices
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' multiplex.density(M)
#' @export

multiplex.density  <- function(multiplex){
  # return a list of density measure in each layer
  density <- vector("integer")
  for(layer in multiplex@layers){
    density <- c(density,graph.density(layer))
  }

  return(density)

}
