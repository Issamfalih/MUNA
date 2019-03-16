
#' Diameter of a multiplex network.
#'
#' @description The diameter of a multiplex network is the average diameter over each layer.
#' @usage diameter_multiplex(multiplex)
#' @param multiplex : The multiplex object.
#' @return numeric constant.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' diameter_multiplex(M)
#' @export

diameter_multiplex <- function(multiplex){
  diameter <- 0
  for(layer in multiplex@layers)
   diameter <- diameter + diameter(layer,directed = FALSE)

  return(round(diameter/length(multiplex@layers)))
}
