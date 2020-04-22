#' Dijkstra's Algorithm.
#'
#' \code{dijkstra} Finds the shortest path from the indicated source node ("init_node") to every adjacent node.
#' Dijkstra's algorithm begins by creating a vector of distances. The source node is marked with a distance of 0 and the rest with infinity (\code{Inf}).  
#' For each neighboring node, the distance will be calculated and compared with the value in the vector of distances. If the current distance is smaller than the distance in the vector will be changed. 
#' When each neighbor node distance has been evaluated, the source node will be marked as visited. This procedure will be repeated until all the nodes in the graph are visited. 
#' 
#' 
#' @param graph an object of class "\code{data.frame}" containing three columns: "v1", "v2" (the nodes of the graph) and "w" (the distance from "v1" to "v2").
#' @param init_node numeric scalar object.
#'
#' @return \code{dijkstra} returns a vector with the shortest path distances from the starting node to every other node.
#'
#' @examples
#' data(wiki_graph)
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#' @export
#' 

dijkstra <- function(graph=wiki_graph, init_node){
  # Set of all nodes
  nodes_set <- 1:max(graph[,1])
  
  # Ensure that the structure and class of the arguments are correct:
  stopifnot(is.numeric(init_node), init_node %in% nodes_set, is.data.frame(graph),names(graph) == c("v1","v2","w" ))
  
  # Inicialization
  dist <- rep(Inf, max(graph[,1])) # initial distances from init_node to the other nodes n is fixed as infinity
  dist[init_node] <- 0             # distance from init_node to init_node is fixed as 0.
  unvisited <- nodes_set           # vector of unvisited nodes
  alt <- vector()                  # vector if alternative distances
  
  # While loop - principal loop
  while (length(unvisited)>0){     # while there are still nodes to visit
    
    u = unvisited[which(dist[unvisited] == min(dist[unvisited]))][1] # unvisited node with the minimum distance
    neighbors <- intersect(graph[graph$v1==u,2],unvisited) # vector of neighbor nodes of the current vector u
    
    # For loop
    for (v in neighbors){   # for each neighbor v
      alt <- dist[u] + graph[which(graph$v1==u & graph$v2==v),3] # Calculation of the alternative distance from u to v
      
      if (alt < dist[v]){ # if the alternative distance is smaller thant the current distance in dist vector
        dist[v] <- alt    # Actualize distance
      }
    }
    unvisited <- unvisited[-(which(unvisited==u))]  # node u is now visited
  }
  return(dist)
}
