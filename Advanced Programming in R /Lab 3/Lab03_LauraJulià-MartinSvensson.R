euclidean <- function(a,b){
  stopifnot(is.numeric(a),is.numeric(b))
  k = 2
  q= vector(); r= vector()
  q[1] = a %/% b
  r[1] = a %% b
  while (tail(r,1) != 0){
    a=b
    b=r[k-1]
    q[k] = a %/% b
    r[k] = a %% b
    k = k + 1  
  }
  return(b)
}


dijkstra <- function(graph=wiki_graph, init_node=1){
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

wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                       v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                       w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)
