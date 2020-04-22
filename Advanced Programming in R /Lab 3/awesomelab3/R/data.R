#' Graph of nodes and distances between nodes.
#'
#' A dataset containing the nodes of a graph and the distances between them
#' (it is the first graph at the wikipedia page, see References).
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{a node of the graph}
#'   \item{v2}{a node of the graph, adjacent node of v1}
#'   \item{w}{the distance of the path between v1 and v2}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Graph_theory}
"wiki_graph"

utils::globalVariables(c("wiki_graph"))