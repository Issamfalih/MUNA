

#' Licod community detection algorithm
#'
#' @description This applies the Licod algorithm as described in VCSJ 2014 (Yakoubi and Kanawati, 2014)
#' @usage community.licod(graph, sigma, centrality1, centrality2,delta,  vote_fun, sim_fun, memb_fun, verbose)
#' @param verbose : verbose
#' @param memb_fun : is the membership function i.e mean, sum, ...
#' @param sim_fun : is the similarity function. i.e similarity.invlogweighted, similarity.jaccard, similarity.dice,...
#' @param vote_fun : The vote function that should be used to compute the membership vector.
#' @param delta : is a threshold in [0; 1]. Two leaders are linked if their topological similarity is above delta.
#' @param centrality2 : is a topological centrality in graphs as degree, closeness, betweenness...
#' @param centrality1 : is a topological centrality in graphs as degree, closeness, betweenness...
#' @param sigma : is a threshold in [0; 1]. It is used to know if a node is a leader.
#' @param graph : The input igraph graph
#' @return returns an igraph  communities object, please see igraph manual page for details.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @references
#' Yakoubi, Z. et R. Kanawati (2014). Licod : Leader-driven approaches for community detection. Vietnam Journal of Computer Science - Springer 1, 241-256.
#' @examples
#' g <- graph.famous("Zachary")
#' wt <- community.licod(g)
#'  V(g)$color <- wt$membership+1
#'  g$layout <- layout.fruchterman.reingold
#'  plot(g, vertex.label=NA)
#' @export

community.licod <- function(graph, sigma=0.9, centrality1=degree, centrality2=similarity.jaccard,delta=0.9,vote_fun=rankBorda, sim_fun=similarity.dice, memb_fun=mean, verbose= FALSE){

  if(!is.igraph(graph))
    stop("Should be an igraph graph")
  if(verbose)
    print(paste("start ",length(V(graph))))

  #works on a copy of graph
  net  <- graph
  if(verbose)
    print(paste("connected graph : ", is.connected(net)))

  if(is.null(get.vertex.attribute(net,"name"))){
    print("No vertex name attribute")
    print("Proceeding by naming vertex automatically")
    net <- set.vertex.attribute(net,"name", index=V(net),value = c(as.character(V(net))))
  }

  if(is.connected(net)&&(length(get.vertex.attribute(net,"name"))==vcount(net))&&(vcount(net)>3)){

    #identify leaders
    nodes_leader  <- compute_node_leaders(net,centrality1, sigma)
    if(verbose){
      print(paste(length(nodes_leader), "leader-nodes found"))
    }
    if(length(nodes_leader)==0){
      stop("No node leaders found")
      # to be done later returns a default community structure where each node is a community
    }

    #group leaders that share more than delta neighbors

    communitiesLeader <- compute_leaders(net,nodes_leader,centrality2,delta)
    strleaders <- vector()
    for(i in seq(length(communitiesLeader)))
      strleaders <- c(strleaders,paste("C",i,sep=""))
    names(communitiesLeader) <- strleaders
    if(verbose){
      print(paste(length(communitiesLeader), "communities-leader found"))
    }
    vertex_names <-  V(net)$name


    old_com <- as.integer(c(vertex_names))
    names(old_com) <- vertex_names

    dis <- distances(net)
    rownames(dis) <- vertex_names
    colnames(dis) <- vertex_names
    if(verbose){
      print(dis)
    }

      for(v in vertex_names){
        tmp <- vector(length=length(communitiesLeader))
        names(tmp) <- strleaders

        for(leader in strleaders)
          tmp[leader] <- memb_fun(dis[v,communitiesLeader[[leader]]])
        net <- set.vertex.attribute(net,name = "membership",index = v,value = list(c(names(sort(tmp)))))
      }

      for(v in vertex_names){
        mv <- matrix(nrow = (length(neighbors(net,v))+1), ncol = length(communitiesLeader))
        mv[1,] <- get.vertex.attribute(net,name = "membership",index = v)[[1]]
        nv_index <- 2
        for(nv in neighbors(net,v)){
          mv[nv_index,] <- get.vertex.attribute(net,name = "membership",index = nv)[[1]]
          nv_index <- nv_index +  1
        }

        net <- set.vertex.attribute(net,name = "new_membership",index = v,value = list(c(rankBorda(mat = mv))))
      }

      net <- set.vertex.attribute(net,name = "membership",index = vertex_names,value = list(get.vertex.attribute(net,name = "new_membership",index = vertex_names))[[1]])

      community <- vector(length = length(vertex_names))
      names(community) <- vertex_names
      for(v in vertex_names){
        community[v] <- as.integer(strsplit(get.vertex.attribute(net,"membership",index=v)[[1]][1],"C")[[1]][2])
      }

      for(l in 1:length(unique(community))){
        community[which(community == unique(community)[l])] <- l
      }
      com2memb <- create.communities(graph = net, membership = community,algorithm="Licod")
    return(com2memb)
  }
  else if(!is.connected(net)){
    # apply Licod algorithm on each connected subgraph
    com <- list()
    for(clust in groups(clusters(net))){
      if(length(clust)<3)
        com <- c(com,list(clust))
      com <- c(com, groups(community.licod(induced.subgraph(net,clust), sigma, centrality1, centrality2, delta,vote_fun, sim_fun, memb_fun, verbose)))
}
    return(com)
  }
  else if(vcount(net)<=3){
    #each vertex is a community
    com <- list()
    for(v in V(net)$name)
      com <- c(com, list(as.integer(v)))
    if(verbose)
      print(paste("com ",com))
    return(com)
  }
}







compute_node_leaders <- function(graph,centralityFunction, sigma){
  #returns a graph with leaders nodes marked as leaders: attribute leader<-True
  leader_nodes <- vector()
  graph <- set.vertex.attribute(graph,name="Centrality",index = V(graph),value = centralityFunction(graph))
  for(v in get.vertex.attribute(graph, name="name")){
    taux =  0
    for(neighbor in neighbors(graph = graph,v = as.character(v))){
      if(get.vertex.attribute(graph = graph,name = "Centrality",index = neighbor) > get.vertex.attribute(graph = graph,name = "Centrality",index = as.character(v))){
        taux =  taux + 1.0
      }
    }
    if((taux/(1+length(neighbors)))>sigma)
      graph <- set.vertex.attribute(graph,name="leader",index = as.character(v),value = TRUE)
    else
      graph <- set.vertex.attribute(graph,name="leader",index = as.character(v),value = FALSE)
  }
  return(as.vector(names(V(graph)[which(V(graph)$leader==TRUE)])))
}




compute_leaders <- function(graph,nodes_leader,centrality,epsilon){
  #returns a list of list: each sublist constitue a leader (a bunch of nodes)
  res <- list()
  n = length(nodes_leader)
  # mat is a similarity matrix
  mat <- as.matrix(centrality(graph,nodes_leader))
  sub_g <- epsilon_threshold_graph(mat,epsilon,names=as.vector(nodes_leader))
  for(gr in decompose.graph(sub_g)){
    res <- c(res, list(V(gr)$name))
  }
  return(res)
}




