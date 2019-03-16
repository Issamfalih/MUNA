
#' The number of edges in the multiplex network
#'
#' @description The number of edges in the multiplex network per layer
#' @usage  ecount.multiplex(multiplex)
#' @param multiplex : The multiplex object.
#' @return  A list of the number of edges in each layer.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' ecount.multiplex(M)
#' @export


ecount.multiplex <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  res <- vector(mode = "integer")
  layers = get_number_of_layers.multiplex(multiplex)
  if(layers<=0)
    return(NULL)
  for(i in seq(layers)){
    res <- c(res, ecount(multiplex@layers[[i]]))
  }
  return(res)
}
