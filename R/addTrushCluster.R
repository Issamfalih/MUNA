#' Add a trush cluster
#'
#' @description Add a trush cluster
#' @usage add_trushcluster(memb)
#' @param memb : A membership partition list
#' @return Multiplex network object, whith the edge added
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#'
#' @export


add_trushcluster <- function(memb){
    trush <- vector()
    membership <- list()
    for(com in memb){
      if(length(com)<=2)
        trush <- c(trush,com)
      else
        membership <- c(membership,list(com))
    }
    if(length(trush)!=0)
      membership <- c(membership, list(trush))
    return(membership)
}
