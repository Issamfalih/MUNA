#'  Rank Aggregation via Borda algorithm
#'
#' @description Rank aggregation of ordered lists is performed using the Borda algorithm.
#' @usage rankBorda(mat,decreasing)
#' @param mat : a matrix of ordered lists to be combined (lists must be in rows)
#' @param decreasing : logical. Should the sort be increasing or decreasing?
#' @return The aggregated list.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' matt <- matrix(c("A", "B", "C", "D", "E",
#' "B", "D", "A", "E", "C",
#' "B", "A", "E", "C", "D",
#' "A", "D", "B", "C", "E"), byrow=TRUE, ncol=5)
#' print(rankBorda(matt))
#' @export
rankBorda <- function(mat,decreasing=TRUE){

  if(!is.matrix(mat))
    stop("Please give a matrix")
  score <- ncol(mat)
  dicScore <- vector(length = score)
  names(dicScore) <- as.vector(mat[1,])
  for(i in seq(nrow(mat))){
    for(j in seq(ncol(mat))){
      dicScore[mat[i,j]] <- score - j + 1

    }
  }

  return(names(sort( dicScore,decreasing=decreasing)))

}
