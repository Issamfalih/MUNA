

#' Epsilon-threshold graph
#'
#' @description Epsilon-threshold graph. Two nodes are connected if their similarity is above
#   the given epsilon or if their distance is below the threshold
#' @usage epsilon_threshold_graph(mat,epsilon,names,similar)
#' @param mat : distance or similarity matrix. Should be square.
#' @param similar : logical scalar. If true matrix mat is a smiliarity matrix. Otherwise, mat is a distance matrix.
#' @param names : character list. The names of vertices.
#' @param epsilon : numeric in [0,1].
#' @return Returns an igraph graph
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' d <-  dist(iris)
#' epsilon_threshold_graph(as.matrix(d),epsilon=0.5)
#'
#' @export

epsilon_threshold_graph <- function(mat,epsilon,names=NULL,similar = TRUE){

    if((nrow(mat)==ncol(mat))&& (epsilon<= 1) &&(epsilon >=0)){
    g <- graph.empty(directed = FALSE)
    g <- add.vertices(g,nv = nrow(mat))
    if(is.null(names)||(length(names)!=nrow(mat)))
      g <- set.vertex.attribute(g,name="name", index = V(g), value=c(as.character(V(g))))
    else
      g <- set.vertex.attribute(g,name="name", index = V(g), value=names)
    if(nrow(mat)==1)
      return(g)
    for(i in 1:(ncol(mat)-1)){
      for(j in ((i+1) : ncol(mat))){
        if(similar){
          if(mat[i,j]>=epsilon)
            g <- add.edges(graph = g,edges = c(i,j),weight=mat[i,j])
        }
        else{
          if(mat[i,j]<=epsilon)
            g <- add.edges(graph = g,edges = c(i,j),weight=mat[i,j])
        }
      }
    }
    return(g)
  }
  else
    stop("non square matrix or illegal threshold value (should be in [0,1])")
}
