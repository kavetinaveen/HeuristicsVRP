#' To find near optimal solutions using Simmulated Annealing.
#' @param fitness Fitness function
#' @param vc Vehicle capacity
#' @param demand Demand at each node
#' @param location Location of each node
#' @param method -- Metric to calculate distnace between nodes. Feasible methods for X-Y co-ordinates c("euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"); Feasible methods for Long-Lat c(distCosine, distHaversine). Default: "euclidean". Note: Please make sure that, method should be character for X-Y co-ordinates and not for Long-Lat.
#' @param popSize Population Size
#' @param nrun Number of runs
#' @examples 
#' popSize <- 100
#' num_nodes <- 31
#' fitness <- fitness
#' vc <- 100
#' demand <- An32k5demand
#' locations <- An32k5locations
#' DMat <- DistMat(locations)
#' row.names(DMat) <- c(0, 1:(nrow(demand)-1))
#' colnames(DMat) <- c(0, 1:(nrow(demand)-1))
#' SA_VRP(fitness, vc, demand, locations, popSize = 100)
#' SA_VRP(fitness, vc, demand, locations)
#' @export 

SA_VRP <- function(fitness, vc, demand, locations, method = "euclidean", popSize = NULL, nrun = 25){
  num_nodes <- nrow(locations)-1
  solution <- sample(num_nodes)
  DMat <- DistMat(locations, method)
  row.names(DMat) <- c(0, 1:(nrow(demand)-1))
  colnames(DMat) <- c(0, 1:(nrow(demand)-1))
  old_cost <- fitness(solution, vc, demand, DMat)
  sol <- c()
  Temp <- 1.0
  alpha <- 0.9
  # k <- 0
  # k2 <- 0
  all_costs <- c()
  # previous_cost <- 0
  # prev <- 0
  if(!is.null(popSize)){
    Temp_Min <- 0.9^popSize
  }else{
    Temp_Min <- 0.9^100
  }
  while(Temp > Temp_Min){
    i <- 1
    while(i <= 100){
      new_solution <- generate_neighbor(solution)
      new_cost <- fitness(new_solution, vc, demand, DMat)
      ap <- acceptance_probability(old_cost, new_cost, Temp)
      # cat(ap, "\n")
      if(ap > runif(1)){
        solution <- new_solution
        old_cost <- new_cost
      }
      i <- i + 1
    }
    cat(old_cost, "\n")
    all_costs <- c(all_costs, old_cost)
    # if(prev == old_cost){
    #   k <- k + 1
    # }else{
    #   k <- 0
    # }
    # if(k == 10){
    #   cat("I'm here... \n")
    #   k2 <- k2 + 1
    #   Temp = 0.7
    # }else{
    #   Temp <- Temp * alpha
    # }
    # if(k2 >= 5) Temp <- Temp * alpha
    # prev <- old_cost
    sol <- rbind(sol, solution)
    Temp <- Temp * alpha
  }
  sol <- as.matrix(sol)
  colnames(sol) <- NULL
  row.names(sol) <- NULL
  if(is.null(popSize)){
    sol <- sol[which.min(all_costs), ]
  }
  return(sol)
}
