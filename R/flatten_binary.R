#' Binary layer aggregation
#'
#' @description Binary aggregation of all layers in the multiplex network : Edges which are included in at least one graph will be part of the new graph ( the aggregated graph).
#' @usage flatten_binary(multiplex)
#' @param multiplex : The multiplex object.
#' @return A new igraph graph object.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' g <- flatten_binary(M)
#' summary_graph(g)
#' @export

flatten_binary <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  return(simplify(graph.union(multiplex@layers,byname = TRUE),remove.multiple = TRUE,remove.loops = TRUE))
}
