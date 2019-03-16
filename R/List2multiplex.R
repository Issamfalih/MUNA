
#' adjacency matrix list to multiplex
#'
#' @description Convert a list of adjacency matrix to a multiplex network
#' @usage adjList2multiplex(list,nodesName)
#' @param list : A list of adjacency matrix of the same length
#' @param nodesName : Vector that contains node's name.
#' @return a multiplex object
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' adjMultiplex <- get_adjacency.multiplex(Multiplex_CKM_Physicians_Innovation)
#' adjList2multiplex(adjMultiplex)
#' @export
#'

#I can used add layer
# Add the name of graphs and nodes later
adjList2multiplex <- function(list,nodesName=NULL){
  if(length(list)==0)
    stop("'list' should not be empty")
  for(i in seq(length(list))){
    if(!is.matrix(list[[i]])&&!(is(list[[i]],"dgCMatrix")))
      stop("'list' should a list of matrix")
    if((nrow(list[[1]])!=ncol(list[[i]])) || (nrow(list[[1]])!=nrow(list[[i]]) ))
      stop("'list' should be a list of square matrix with the same length")
  }
  self <- new("Multiplex")
  nbLayers=length(list)
  if(! is.vector(list,"list"))
    stop("not a list ")
  if(is.null(nodesName))
    self@nodes=as.character(seq(nrow(list[[1]])))
  else if(length(nodesName)!=nrow(list[[1]])){
    stop("nodesName should have the same length as matrix")
  }else{
    self@nodes=nodesName}


  for(i in seq(length(list))){
    self@layers[[i]] <- simplify(graph.adjacency(list[[i]],mode = "undirected"),remove.multiple = TRUE,remove.loops = TRUE)
    self@layers[[i]] <- set.vertex.attribute(self@layers[[i]], name = "name",value = self@nodes)
  }
  return(self)
}
