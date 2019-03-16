#' MuxLicod community detection algorithm
#'
#' @description This applies the MuxLicod algorithm as described in EGC 2015 (I.Falih, M.Hmimida, and R.Kanawati)
#' @usage community.muxLicod(multiplex,sigma,centrality1,centrality2,delta,vote_fun,sim_fun,memb_fun,verbose)
#' @param verbose : verbose
#' @param memb_fun : is the membership function i.e mean, sum, ...
#' @param sim_fun : is the similarity function. i.e similarity.invlogweighted, similarity.jaccard, similarity.dice,...
#' @param vote_fun : The vote function that should be used to compute the membership vector.
#' @param delta : is a threshold in [0; 1]. Two leaders are linked if their topological similarity is above delta.
#' @param centrality2 : is a topological centrality in graphs as degree, closeness, betweenness...
#' @param centrality1 : is a topological centrality in graphs as degree, closeness, betweenness...
#' @param sigma : is a threshold in [0; 1]. It is used to know if a node is a leader.
#' @param multiplex : The input multiplex network
#' @return returns an igraph  communities object, please see igraph manual page for details.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @references
#' Issam Falih, Manel Hmimida, Rushed Kanawati, Une approache centr\'ee graine pour la d\'etection de communaut\'es dans les r\'eseaux multiplexes. Conf\'erence francophone sur l'Extraction et la Gestion de Connaissance (EGC'15), 27-30 Jan. 2015 Luxembourg.
#' #' @examples
#' M <- Multiplex_CKM_Physicians_Innovation
#' community.muxLicod(M)
#' @export

community.muxLicod <- function(multiplex,sigma=0.9, centrality1=centrality_closeness.multiplex,centrality2=similarity.jaccard,delta=0.9,vote_fun=rankBorda, sim_fun=similarity.dice, memb_fun=mean, verbose= FALSE){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")

  #identify leaders
  nodesCentralityList <- centrality1(multiplex)
  names(nodesCentralityList) <- multiplex@nodes
  nodes_leader  <- compute_node_leaders_multiplex(multiplex,nodesCentralityList, sigma)

  if(verbose){
    print(paste(length(nodes_leader), "leader-nodes found"))
  }
  if(length(nodes_leader)==0){
    stop("No node leaders found")
    # to be done later returns a default community structure where each node is a community
  }

  #group leaders that share more than delta neighbors

  communitiesLeader <- compute_leaders_multiplex(multiplex,nodes_leader,centrality2)
  strleaders <- vector()
  for(i in seq(length(communitiesLeader)))
    strleaders <- c(strleaders,paste("C",i,sep=""))
  names(communitiesLeader) <- strleaders
  if(verbose){
    print(paste(length(communitiesLeader), "communities-leader found"))
  }
  vertex_names <-  multiplex@nodes

  net <- flatten_redundancy(multiplex)
  #net <-delete.edges(net,E(net)[E(net)$weight<round(length(multiplex@layers)*0.75)])

  dis <- distances(net,to = nodes_leader)
  dis[dis==Inf] <- diameter(net)

  rownames(dis) <- vertex_names
  colnames(dis) <- nodes_leader
  tmp <- matrix(0,nrow=length(vertex_names),ncol=length(strleaders))

  rownames(tmp) <- vertex_names
  colnames(tmp) <- strleaders
  for(v in vertex_names){
  for(leader in strleaders){
        tmp[v,leader] <- memb_fun(dis[v,communitiesLeader[[leader]]])
        net <- set.vertex.attribute(graph = net,name = "membership",index = v,value = list(names(sort(tmp[v,]))) )
  }
  }

  #print(get.vertex.attribute(net,"membership"))
  for(v in vertex_names){
    mv <- matrix(unlist(get.vertex.attribute(net,name = "membership",index = c(v,v,v,names(neighbors(net,v))))),ncol=length(strleaders),byrow = TRUE)
    net <- set.vertex.attribute(net,name = "new_membership",index = v,value = list(rankBorda(mat = mv)))
  }
  net <- set.vertex.attribute(net,name = "membership",index = vertex_names,value = get.vertex.attribute(net,name = "new_membership",index = vertex_names))

  community <- vector(length = length(vertex_names))
  names(community) <- vertex_names
  for(v in vertex_names){
    community[v] <- as.integer(strsplit(get.vertex.attribute(net,"membership",index=v)[[1]][1],"C")[[1]][2])
  }

  for(l in 1:length(unique(community))){
    community[which(community == unique(community)[l])] <- l
  }
  return(community)

}



compute_node_leaders_multiplex <- function(multiplex, nodesCentralityList,sigma){
  leader_nodes <- vector()
  flatten_graph = flatten_binary(multiplex)
  for(v in multiplex@nodes){
    neighborss <- neighbors(flatten_graph,v)
    if(length(neighborss)==0) next
    if((length(which(nodesCentralityList[neighborss]<=nodesCentralityList[v]))/length(neighborss))>=sigma)
      leader_nodes <- c(leader_nodes,as.character(v))
  }
  return(unique(leader_nodes))
}

neighbors_all.multiplex <- function(multiplex,delta){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")

  net <- flatten_redundancy(multiplex)
  E(net)$weight = E(net)$weight>round(length(multiplex@layers)*delta)
  neighMux=list()
  for( node_name in V(net)$name){
    neighb <- neighbors(net,node_name)
    neighMux <- c(neighMux, list(as.vector(neighb)))
  }
  names(neighMux) <- V(net)$name
  return(neighMux)

}

neighbors_all_similarity.multiplex <- function(multiplex,delta, similarity_function = similarity.jaccard ){
    if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
    flatten_graph <- flatten_binary(multiplex)
    neighMux=list()
    simMat <- similarity_function(flatten_graph,mode = "all",loops = FALSE)
    simMat <- simMat/max(simMat)
    rownames(simMat) <- V(flatten_graph)$name
    colnames(simMat) <- V(flatten_graph)$name
    for( node_name in multiplex@nodes){

      neighb <- neighbors(flatten_graph,node_name)
      if(length(neighb)==0) next
      if(length(which(simMat[node_name,neighb] >= delta))!=0)
        neighMux <- c(neighMux, list(node_name= neighb[which(simMat[node_name,neighb] >= delta)]))
    }
    return(neighMux)

}

compute_leaders_multiplex <- function(multiplex,nodes_leader,centrality){
  #returns a list of list: each sublist constitue a leader (a bunch of nodes)
  res <- list()
  n = length(nodes_leader)
  # mat is a similarity matrix
  mat <- matrix(0,nrow = n,ncol=n)
  for(layer in multiplex@layers)
   mat = mat + as.matrix(centrality(layer,nodes_leader))
  #normalisation
  mat <- mat/max(mat)
  sub_g <- epsilon_threshold_graph(mat,epsilon=0.75,names=as.vector(nodes_leader))
  for(gr in decompose.graph(sub_g)){
    res <- c(res, list(V(gr)$name))
  }
  return(res)
}

