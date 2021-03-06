
#' convert a bipartite network to two multiplex network.
#'
#' @description convert a bipartite network to two multiplex network by projecting the first subset on the second subset. And projecting the second subset on the first subset.
#' @usage bipartite2multiplex(edgeList,subset1,subset2,rankList, threshold1, threshold2, weighted)
#' @param weighted : logical. whether to include the weight generated by the projection.
#' @param threshold2 : numeric in [0,1]. The minimum threshold to consider a link in the second projection
#' @param threshold1 : numeric in [0,1]. The minimum threshold to consider a link in the first projection
#' @param rankList : a integer vector. e.g (1:5)
#' @param subset2 : vector. The second subset (bottom)
#' @param subset1 : vector. The first subset  (top)
#' @param edgeList : The edge list, a two column matrix, character or numeric
#' @return a list of two multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @export
bipartite2multiplex <- function(edgeList,subset1,subset2,rankList, threshold1, threshold2, weighted){
    A <- vector("list",length(rankList))
    names(A) <- rankList
    for(i in rankList){
      A[[i]] <- matrix(c(0),nrow = length(subset1),ncol = length(subset2))
      rownames(A[[i]]) <- subset1
      colnames(A[[i]]) <- subset2
      for(j in which(as.vector(edgeList[,3])==i))
        A[[i]][edgeList[j,1],edgeList[j,2]] <- 1
    }

  AA <- vector("list",length(rankList))
  names(AA) <- rankList
  for(i in rankList){
    AA[[i]] <- matrix(c(0),nrow = length(subset1),ncol = length(subset1))
    AA[[i]] <-  A[[i]] %*%  t(A[[i]])
  }
j=1
  for(i in rankList){
    if(is.vector(threshold1)){
      AA[[i]] <- threshold_matrix(AA[[i]], threshold1[j],weighted)
      j=j+1
    }
    else{
      AA[[i]] <- threshold_matrix(AA[[i]], threshold1,weighted)

    }
  }
  M1 <- adjList2multiplex(AA,subset1)

  AA <- vector("list",length(rankList))
  names(AA) <- rankList
  for(i in rankList){
    AA[[i]] <- matrix(c(0),nrow = length(subset2),ncol = length(subset2))
    AA[[i]] <-  t(A[[i]]) %*%  A[[i]]
  }

  j=1
  for(i in rankList){
    if(is.vector(threshold2)){
      AA[[i]] <- threshold_matrix(AA[[i]], threshold2[j],weighted)
      j=j+1
    }
    else{
      AA[[i]] <- threshold_matrix(AA[[i]], threshold2,weighted)

    }
  }
  M2 <- adjList2multiplex(AA,subset2)

  return(list(M1,M2))
}


threshold_matrix <- function(mat, threshold,weighted){
  A <- mat
  A[A<threshold] <- 0
  if(!weighted)
    A[A>=threshold] <- 1
  return(A)
}


threshold.matrix <- function(mat, threshold,weighted){
  A <- mat
  A[A<threshold] <- 0
  if(!weighted)
    A[A>=threshold] <- 1
  return(A)
}


# bipartite2multiplexSoumaya <- function(edgeList,sousensemble1,sousensemble2){
#
#
#   M1 <- new("Multiplex")
#   M2 <- new("Multiplex")
#   M1@nodes <- c(sousensemble1)
#   M2@nodes <- c(sousensemble2)
#
#   A <- matrix(c(0),nrow = length(sousensemble1),ncol = length(sousensemble2))
#   rownames(A) <-sousensemble1
#   colnames(A) <- sousensemble2
#
#   A[as.character(edgeList[,1]),as.character(edgeList[,2])] <- 1
#
#   AA <- matrix(c(0),nrow = length(sousensemble1),ncol = length(sousensemble2))
#   AA <- A  %*%  t(A)
#
#   g1 <- graph.adjacency(AA,mode = "undirected")
#   g1 <- set.vertex.attribute(g1,"name",index = V(g1),value = M1@nodes)
#   M1@layers[[length(M1@layers)+1]] <- g1
#
#   AA <- matrix(c(0),nrow = length(sousensemble1),ncol = length(sousensemble2))
#   AA <- t(A)  %*%  A
#   g2 <- graph.adjacency(AA,mode = "undirected")
#   g2 <- set.vertex.attribute(g2,"name",index = V(g2),value = M2@nodes)
#   M2@layers[[length(M2@layers)+1]] <- g2
#
#
#   return(list(M1,M2))
# }





#
#
#
# bipartite2matrix <- function(edgeList,subset1,subset2,rankList, threshold, weighted){
#   A <- vector("list",length(rankList))
#   names(A) <- rankList
#   for(i in rankList){
#     A[[i]] <- matrix(c(0),nrow = length(subset1),ncol = length(subset2))
#     rownames(A[[i]]) <-subset1
#     colnames(A[[i]]) <- subset2
#     for(j in which(as.vector(edgeList[,3])==i))
#       A[[i]][as.character(edgeList[j,1]),as.character(edgeList[j,2])] <- 1
#   }
#
#
#   AA <- vector("list",length(rankList))
#   names(AA) <- rankList
#   for(i in rankList){
#     AA[[i]] <- matrix(c(0),nrow = length(subset1),ncol = length(subset2))
#     AA[[i]] <-  A[[i]] %*%  t(A[[i]])
#   }
#
#   for(i in rankList)
#     AA[[i]] <- threshold_matrix(AA[[i]], threshold[i],weighted)
#
#   return(AA)
# }
#

#
# bipartite2multiplex <- function(fileName,nbDim1,nbDim2,nbTags,dim1,dim2,dimTags,Tags,delta){
#   # this function return a list of matrix
#   data <- read.table(file = fileName)
#   A <- vector("list",nbTags)
#   for(i in seq(nbTags)){
#     A[[i]] <- matrix(c(0),nrow = nbDim1,ncol = nbDim2)
#     for(j in which(data[dimTags]==Tags[i])){
#       A[[i]][data[j,dim1],data[j,dim2]] <- 1
#     }
#   }
#   AA <- vector("list",nbTags)
#   for(i in 1:nbTags){
#     AA[[i]] <- matrix(c(0),nrow = nbDim1,ncol = nbDim1)
#     AA[[i]] <-  A[[i]] %*%  t(A[[i]])
#   }
#   return(AA)
# }
