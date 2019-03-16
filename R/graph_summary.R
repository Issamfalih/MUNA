
#' Summary of the graph
#'
#' @description The summary of the input graph
#' @usage summary_graph(graph)
#' @param graph : an igraph graph
#' @return A list of graph description
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' g = graph.famous("Zachary")
#' summary_graph(g)
#' @export
summary_graph <- function(graph){
  if(!is.igraph(graph))
    stop("Graph should be an igraph graph")

  str = paste("Number of nodes : ",vcount(graph),"\n",
              "Number of edges : ",ecount(graph),"\n",
              "Graph density : ",round(graph.density(graph),4),"\n",
              #"Graph diameter : ",diameter(graph),"\n",
              "Graph transitivity : ",round(transitivity(graph),4),"\n",
              "average degree : ",round(mean(degree(graph)),4),"\n",
              "min degree : ",round(min(degree(graph)),4),"\n",
              "max degree : ",round(max(degree(graph)),4),"\n"
              #"Graph transitivity : ",round(transitivity(graph),4),"\n"
  )
  if(!is.connected(graph)){
    str = paste0(str ,"The graph is unconnected ","\n \t",
                 "Number of connected components : ",clusters(graph)$no,"\n\t",
                 "Greastest connected component length : ", sort(clusters(graph)$csize, decreasing = TRUE)[1],
                 " it represents ",round(sort(clusters(graph)$csize, decreasing = TRUE)[1]/vcount(graph) * 100,2)," of graph size" ,"\n")
  }
  return(str)
}
