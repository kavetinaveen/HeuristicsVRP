#' Rank space selection operator with diversity
#' @param fit Fitness values of population
#' @param edge_dist Distance between two connected nodes
#' @param pc Crossover probability
#' @param p Weight for diversification
#' @examples 
#' 
#' @export

selection_RS_Diversity <- function(fit, edge_dist, pc = 0.6, p = 0.2){
  rank <- (length(fit) + 1) - rank(fit, ties.method = "random")
  prob <- ((1 - pc)^(rank-1)) * pc
  sel <- selection_diversity(prob, edge_dist, diversity = rep(0, length(prob)), sel = NULL, pc, p, 1)
  return(sel)
  # out <- list(population = object@population[sel, ], fitness = object@fitness[sel])
}