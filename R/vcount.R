#' The number of vertices in the multiplex network
#'
#' @description The number of vertices in the multiplex network
#' @usage  vcount.multiplex(multiplex)
#' @param multiplex : A multiplex object.
#' @return Numeric scalar, the number of vertices.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' vcount.multiplex(M)
#' @export

vcount.multiplex <- function(multiplex){
  # return the number of nodes in the graph
  #all layers have the same number of nodes
  sapply(multiplex@layers, function(graph){if(vcount(graph)!=length(multiplex@nodes)) stop("The number of nodes in multiplex object is not the same")})
  return(length(multiplex@nodes))
}
