


#'  Partition aggregation based community detection algorithm
#'
#' @description This method applies a partition aggregation based community detection algorithm. The idea is to apply a standard community detection algorithm designed for monoplex network (community_algo) to each layer of the multiples. Then an ensemble clustering approach is applied on the obtained clusterings in order to compute the final community structure. The CSPA ensemble clustering approach is used for that purpose. All basic community detection approaches provided in igraph can be used here.
#' @usage community.partition_aggregation(multiplex, community_algorithm, h, alpha)
#' @param community_algorithm : String, the name of classical community detection algorithm. As Licod, Louvain, Infomap, Walktrap,...
#' @param alpha :
#' @param h :
#' @param multiplex : The multiplex object.
#' @return returns an igraph  communities object, please see igraph manual page for details.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' community.partition_aggregation(M)
#' @export



community.partition_aggregation <- function(multiplex, community_algorithm=cluster_louvain, h=1, alpha=0.75){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  Threshold <- h*alpha
  nbNodes = length(multiplex@nodes)
  mat <- matrix(c(0),ncol=nbNodes,nrow=nbNodes)
  colnames(mat) <- multiplex@nodes
  rownames(mat) <- multiplex@nodes
  res <- matrix(c(0),ncol=nbNodes,nrow=nbNodes)
  colnames(res) <- multiplex@nodes
  rownames(res) <- multiplex@nodes
  for(i in 1:h){
    for(graph in multiplex@layers){
      wt <-community_algorithm(graph)
      comList <- groups(wt)
      for(com in comList){
        if(length(com)>1){
          for( x in seq((length(com)-1))){
            for( y in (x+1):length(com)){
              mat[com[x],com[y]] <- (mat[com[x],com[y]] + 1)
              mat[com[y],com[x]] <- mat[com[x],com[y]]
            }
          }
        }
      }
    }
  }
  for(x in seq((nbNodes-1))){
    for(y in seq((x+1),nbNodes)){
      if( mat[x,y] >= Threshold){
        res[x,y] <- 1
        res[y,x] <- 1
        #res[x,y] <- mat[x,y]
      }
    }
  }

  graph_res <- graph.adjacency(res,mode = "undirected")
  if(is.connected(graph_res))
    return(communities(cluster_louvain(graph_res)))
  clusters <- clusters(graph_res)
  return(communities(clusters))
}


#'  Layer aggregation based community detection algorithm
#'
#' @description This method applies the binary layer aggregation approaches. Then a classical community detection algorithm (one of igraph provided algorithms) can be applied to the resulting monoplex network.
#' @usage community.layer_aggregation(multiplex, flatten_method, community_algorithm)
#' @param community_algorithm : String, the name of classical community detection algorithm. As Licod, Louvain, Infomap, Walktrap,...
#' @param flatten_method : flaten a multiplex network into a simple monoplex network method
#' @param multiplex : The multiplex object.
#' @return returns an igraph  communities object, please see igraph manual page for details.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' community.layer_aggregation (M, flatten_binary, cluster_louvain)
#' community.layer_aggregation (M, flatten_redundancy,cluster_louvain)
#' community.layer_aggregation(M, flatten_binary, cluster_infomap)
#' community.layer_aggregation(M, flatten_binary, cluster_louvain)
#' community.layer_aggregation(M, flatten_binary, cluster_walktrap)
#' community.layer_aggregation(M, flatten_binary, cluster_edge_betweenness)
#' community.layer_aggregation(M, flatten_binary, cluster_label_prop)
#' @export

community.layer_aggregation <- function(multiplex, flatten_method= flatten_binary, community_algorithm=cluster_louvain){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  graph = flatten_binary(multiplex)
    wt = community_algorithm(graph)
    return(communities(wt))
}




#' Community structure detection on each layer
#'
#' @description Find community structure on each layer separately via classical community detection algorithm. As Licod, Louvain, Infomap, Walktrap,...
#' @usage community.by_layer(multiplex,community_algorithm)
#' @param community_algorithm : String, the name of classical community detection algorithm. As Licod, Louvain, Infomap, Walktrap,...
#' @param multiplex : The multiplex object.
#' @return A list of membership of each layer.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' community.by_layer(M)
#' community.by_layer(M,cluster_infomap)
#' community.by_layer(M,cluster_louvain)
#' community.by_layer(M,cluster_walktrap)
#' community.by_layer(M,cluster_edge_betweenness)
#' community.by_layer(M,cluster_label_prop)
#' @export
community.by_layer <- function(multiplex,community_algorithm=multilevel.community){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  P <- list()
  for(layer in multiplex@layers)
    P <- c(P, list(community_algorithm(layer)$membership))
  return(P)
}
