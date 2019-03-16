#' layer similarity of a multiplex network
#'
#' @description layer similarity of a multiplex network
#' @usage similarityPerlayers(multiplex)
#' @param multiplex : The multiplex object.
#' @return A similarity matrix between layer of the multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' similarityPerlayers(M)
#' @export

similarityPerlayers <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  n = length(multiplex@layers)
  mat <- matrix(0,nrow =n, ncol=n)
  names = vector()
  for(i in 1:n){
    names <- c(names,graph_attr(multiplex@layers[[i]])$name)
    for(j in i:n){
      mat[i,j] <- ecount(graph.intersection(multiplex@layers[[i]],multiplex@layers[[j]]))/ecount(graph.union(multiplex@layers[[i]],multiplex@layers[[j]]))
      mat[j,i] <- mat[i,j]
    }
  }
  rownames(mat) = names
  colnames(mat) = names
  return(mat)
  #return(sum(vect)/length(multiplex@layers))
}
