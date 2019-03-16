

#' Short description of the multiplex network
#'
#' @description Short summary of the multiplex network
#' @usage  multiplex.summary(multiplex)
#' @param multiplex : The multiplex object.
#' @return  Character scalar, a short description of the multiplex network. A single sentence
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' multiplex.summary(M)
#' @export

multiplex.summary<- function(multiplex){
  # Returns a string description of the multiplex
  cat("Name : ",multiplex@name, " \n",sep = "")
  cat("Contains ", get_number_of_layers.multiplex(multiplex), " layers and ",length(multiplex@nodes), " nodes \n",sep="")
  for(layer in multiplex@layers){
    cat("\t Layer -- ",get.graph.attribute(layer,"name"), " -- : ", length(E(layer)), " edges \n", sep="")
  }
}
