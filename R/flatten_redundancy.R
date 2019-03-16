#' Redundancy layer aggregation
#'
#' @description return a weighted graph two nodes are linked in the result graph if they are linked i one layer. The edge is weighted by the number of layers in which the two nodes are connected.
#' @usage flatten_redundancy(multiplex, inv=FALSE)
#' @param  inv : Logical, the weight is inverted if inv is TRUE. By default, inv is FALSE
#' @param multiplex : The multiplex object.
#' @return A new weighted igraph graph.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' g <- flatten_redundancy(M)
#' summary(g)
#' @export

flatten_redundancy <- function(multiplex, inv=FALSE){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  n = length(multiplex@nodes)
  A <- matrix(0, ncol = n, nrow=n)
  for(layer in multiplex@layers)
    A <- A + get.adjacency(layer)
  aggr <- graph.adjacency(A, mode="undirected", weighted=TRUE, diag=FALSE)
  aggr <- set.vertex.attribute(aggr,name="name",index=V(aggr),value=multiplex@nodes)
  if(inv){
    aggr <- set.edge.attribute(aggr,"weight",index = E(aggr),value=(1/get.edge.attribute(aggr,"weight",index = E(aggr))))
  }

  return(aggr)
}





# flatten_redundancy <- function(multiplex, inv=FALSE){
#   aggr <- graph.empty(n = length(multiplex@nodes) ,directed = FALSE)
#   aggr <- set.vertex.attribute(aggr,name="name",index = V(aggr),value=c(multiplex@nodes))
#   aggr <- set.edge.attribute(aggr,name="weight",index = V(aggr),value=c(0))
#   for(graph in multiplex@layers){
#     for(edge in get.edgelist(graph))
#     aggr <- add.edges(graph = aggr,edges = )
#     aggr <- set_edge_attr(aggr, "weight", index=as.vector(get.edgelist(graph)), value = c(1.0) + c(get.edge.attribute(aggr,"weight",index = as.vector(get.edgelist(graph)))) )
#   }
# print((get.edge.attribute(aggr,"weight",E(aggr))))
#
#   #aggr <- simplify(aggr, edge.attr.comb = "sum")
#   if(inv){
#     for(e in E(aggr))
#       aggr <- set.edge.attribute(aggr,"weight",index = e,value=(1/get.edge.attribute(aggr,"weight",index = e)))
#   }
#   return(aggr)
# }



