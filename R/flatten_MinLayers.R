

#'layer aggregation
#'
#' @description return an igraph graph two nodes are linked in the result graph if they are linked in a least 'MinLayers'  layer.
#' @usage flatten_MinLayers(multiplex, MinLayers)
#' @param MinLayers : minimal number of layer
#' @param multiplex : The multiplex object.
#' @return A new  igraph graph.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' g <- flatten_MinLayers(M,3)
#' summary_graph(g)
#' @export

flatten_MinLayers <- function(multiplex, MinLayers=3){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  n = length(multiplex@nodes)
  A <- matrix(0, ncol = n, nrow=n)
  for(layer in multiplex@layers)
    A <- A + get.adjacency(layer)
  A[A<MinLayers] <- 0
  return(simplify(graph = graph.adjacency(A, mode="undirected", weighted=NULL, diag=FALSE),remove.multiple = TRUE,remove.loops = TRUE))
}
