
#'  The redundency of each community of a partition in the multiplex network
#'
#' @description The redundency of a partition is the average of the redundant links of each intra-community in all layers. The intuition is that the intra-community link should be recurring in different layers.
#' @usage redundency_partition(multiplex, partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return A list of numeric, the redundency score of each community in a partition.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <- community.partition_aggregation(M)
#' redundency_partition(M, wt)
#' @export
redundency_partition <- function(multiplex, partition){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  return(sum(redundency_partition_byCluster(multiplex, partition),na.rm = TRUE)/length(partition))
}


#'  The redundency of each community of a partition in the multiplex network
#'
#' @description The redundency of a partition is the average of the redundant links of each intra-community in all layers. The intuition is that the intra-community link should be recurring in different layers.
#' @usage redundency_partition_byCluster(multiplex, partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return A list of numeric, the redundency score of each community in a partition.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <- community.partition_aggregation(M)
#' redundency_partition_byCluster(M, wt)
#' @export


redundency_partition_byCluster <- function(multiplex, partition){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  res = vector()
  n = get_number_of_layers.multiplex(multiplex)
  if(n>=1){
    for(cluster in partition){
      mux = subgraph.multiplex(multiplex,as.character(cluster))
      #print(ecount.multiplex(mux))
      if(sum(ecount.multiplex(mux))==0)
        res <- c(res, NA)
      else
        res <- c(res, round(redundency(mux),2))
    }
    return(res)
  }
  else
    stop("Illegal number of layer")
}


#'  The redundency of a multiplex network
#'
#' @description The redundency of a multiplex network
#' @usage redundency(multiplex)
#' @param multiplex : The multiplex object.
#' @return A  numeric scalar, the redundency of the given multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' redundency(M)
#' @export
redundency <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  ec= ecount.multiplex(multiplex)
  d = 0
  for(e in ec){
    if(e>0)
      d = d + 1
  }
  graph = flatten_redundancy(multiplex)
  rho = 0


# for(e in (E(graph)[weight>=2])){
  for(e in which(get.edge.attribute(graph, name="weight")>=2)){
    rho <- rho + get.edge.attribute(graph,"weight", index=e)
  }
  return( rho /(d * ecount(graph)))
}



#'  The similarity between a input igraph graph and all layers in the multiplex network
#'
#' @description The similarity between a input igraph graph and all layers in the multiplex network
#' @usage similarity_layer(multiplex, graph)
#' @param graph : an igraph graph
#' @param multiplex : The multiplex object.
#' @return A  list of numeric scalar,
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' similarity_layer(M, M@layers[[1]])
#' @export
similarity_layer <- function(multiplex, graph){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  vect <- vector()
  for(layer in multiplex@layers)
      vect <- c(vect, (ecount(graph.intersection(layer,graph))/ecount(graph.union(layer,graph))))
  return(vect)
  #return(sum(vect)/length(multiplex@layers))
}

