#' Selection operator using Rank Space Method
#' @param fit Fitness values of population
#' @param pc Crossover probability
#' @examples
#' 
#' @export

selection_RS <- function(fit, pc = 0.5){
  popSize <- length(fit)
  cat(length(unique(fit)), "\n")
  rank <- (popSize + 1) - rank(fit, ties.method = "random")
  prob <- ((1 - pc)^(rank-1)) * pc
  sel <- sample(1:popSize, size = popSize, prob = pmin(pmax(0, prob), 1, na.rm = TRUE), replace = TRUE)
  # out <- list(population = object@population[sel, , drop = FALSE],
  #             fitness = fit[sel])
  return(sel)
}