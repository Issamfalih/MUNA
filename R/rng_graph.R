#'  Relative neighbor graph
#'
#' @description  the relative neighborhood graph (RNG) is an undirected graph defined on a set of points
#'          in the Euclidean plane by connecting two points p and q by an edge whenever there does not exist a third point r
#'                    that is closer to both p and q than they are to each other
#' @usage rng_graph(dx,names,similarity)
#' @param similarity : if TRUE then dx is a similarity matrix else it is a distance matrix
#' @param names : the names of vertex in the new graph
#' @param dx : matrix that express the similarity or distance between elements i and j
#' @return returns the relative neighborhood graph (RNG) defined over the similarity/Distance matrix dx
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' d <-  dist(iris)
#' rng_graph(as.matrix(d))
#' @export

rng_graph <- function(dx, names=NULL,similarity=TRUE) {
  if(similarity)
    fun = are_neighbor_based_similarity
  else
    fun = are_neighbor_based_distance
  n <- nrow(dx)
  if(ncol(dx)==n){
    if(is.null(names) ||(length(names)!=ncol(dx)))
      names <- c(as.character(1:n))

    #   for(i in names)
    #     for(j in names)
    #       if( (i!=j)&&(!are.connected(g,i,j))&& fun(dx,i,j))
    #           g <- add.edges(g,c(i,j),weight=dx[i,j])
    #much better
    n <- nrow(dx)
    A <- matrix(0, nrow = n, ncol = n)
    rownames(A) <- names
    colnames(A) <- names
    for(i in seq(n-1)){
      for(j in (i+1):n){
        if(fun(dx,i,j)){
          A[i, j ] <-  dx[i,j]
        }}}
    out <- simplify(graph.adjacency(A, mode = "undirected",weighted = TRUE),remove.multiple = TRUE,remove.loops = TRUE)
    return(set.vertex.attribute(out, "name", index=V(out), value=names))
  }
  else
    stop("Should be a square dxrix")
}



are_neighbor_based_similarity <- function(matr, vi,vj){
  for(r in 1:ncol(matr)){
    if(max(matr[vi,r],matr[vj,r],na.rm = TRUE)>matr[vi,vj])
      return(FALSE)
  }
  return(TRUE)
}

are_neighbor_based_distance <- function(matr, vi,vj){
  for(r in 1:ncol(matr)){
    if(max(matr[vi,r],matr[vj,r],na.rm = TRUE)<matr[vi,vj])
      return(FALSE)
  }
  return(TRUE)
}


