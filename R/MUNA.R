# library(igraph)
# library(rgl)
# library(mapproj)
# library(maps)
# library(OpenStreetMap)
# library(RankAggreg)

library(utils)

#' Multiplex Class : a class that model the multiplex network.
#'
#' @description Multiplex Class : a class that model multiplex networks.  It contains three slot : name, layers and nodes.
#' @param  name : The name of the multiplex network. The default name is "Multiplex".
#' @param  layers : A list of igraph graphs, each igraph graph is a layer in the multiplex.
#' @param nodes : A vector of vertices of the multiplex network.
#' @import igraph methods
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- new(Class = "Multiplex")
#'
#' @export

setClass (
  # Set the name for the class
  "Multiplex",

  # Define the slots
  slots = c(
    name="character",    # the name of the multiplex class
    layers="list",      #
    nodes="vector"
  ),

  # Set the default values for the slots
  prototype=list(
    name = "Multiplex",
    #nodes=,
    layers=list()
  ),

  # Function to see if the multiplex is consistent
  validity = function(object){
    i = length(object@layers)
    while(i!=0){
      if(length(V(object@layers[[i]])$name)!=vcount(object@layers[[i]])){
        stop (" the number of vertex is not the same in all layers")
      }
    }
    return(TRUE)
  }
)
