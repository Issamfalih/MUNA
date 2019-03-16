
#' Multiplex modularity of a given community structure of a multiplex network
#'
#' @description Multiplex modularity of a given community structure of a multiplex network
#' @usage modularity_multiplex(multiplex, partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return A numeric scalar, the multiplex modularity score of the given configuration
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <- community.partition_aggregation(M)
#' modularity_multiplex(M, wt)
#' @export
modularity_multiplex <- function(multiplex, partition){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  Q <- 0
  u <- sum(ecount.multiplex(multiplex))
  if(!is.null(u)){
    for(layer in multiplex@layers){
      if(is.list(partition))
        memb <- community.to.membership(layer,partition)
      else
        memb <- membership(partition)
      Q <- Q + (modularity(layer,memb)*(2*ecount(layer)))
    }
    Q =  (Q/(2*u))
  }
  return(round(Q,digits = 4))
}

community.to.membership <- function(graph, partition){
  vertex_names <- V(graph)$name
  res <- vector(mode = "integer",length = length(vertex_names))
  memb_index=1
  for(community in partition){
    for(v in community)
      res[which(vertex_names==v)] <-  memb_index
    memb_index= memb_index+1
  }
  return(res)
}



#'  Modularity of a community structure by each layer of a multiplex graph
#'
#' @description  Modularity of a community structure by each layer of a multiplex graph
#' @usage modularity_bylayer.multiplex(multiplex, partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return A list of numeric scalar. The modularity score over each layer of the given configuration.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <- community.partition_aggregation(M)
#' modularity_bylayer.multiplex(M, wt)
#' @export
modularity_bylayer.multiplex <- function(multiplex, partition){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  res <- vector()
  layername <- vector()
  for(layer in multiplex@layers){
    if(is.list(partition))
      memb <- community.to.membership(layer,partition)
    else
      memb <- membership(partition)
    res <- c(res, round(modularity(layer,memb),3))
    layername <- c(layername,layer$name)
  }
  names(res) <- layername
  return(res)
}
