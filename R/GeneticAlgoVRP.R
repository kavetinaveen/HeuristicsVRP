#' Genetic Algorithm structure to solve VRP
#' @param maxitr Maximum number of iterations
#' @param runs If there is no improvement in the fitness value in the consecutive 'runs' iterations then algorithm will terminate
#' @param cp Crossover probability
#' @param mp Mutation probability
#' @param fitness Fitness function
#' @param selection Fuction for selection operator, default selection_RS (Selection with rank space but not with diversity)
#' @param crossover Function for crossover operator
#' @param mutation Function for mutation operator
#' @param init_pop Initial population for GA
#' @param num_nodes Number of customer nodes
#' @param popSize Number of chromozomes in population
#' @export

GA_VRP <- function(maxitr, runs, cp, mp, vc, demand, locations = NULL, DMat = NULL, metric = "euclidean", fitness = fitness, selection = selection_RS, crossover = crossover, mutation = mutation, init_pop = NULL, num_nodes = NULL, popSize = NULL){
  
  if(is.null(init_pop)){
    init_pop <- do.call(rbind, lapply(1:popSize, function(x){sample(num_nodes)}))
  }
  
  if(is.null(init_pop) & (is.null(num_nodes) | is.null(popSize))){
    stop("Please either provide initial population or number of nodes and population size")
  }
  
  if(is.null(locations) & is.null(DMat)){
    stop("Please provide either of locations or distance matrix")
  }
  
  if(is.null(DMat)){
    DMat <- DistMat(locations, method)
  }
  
  if(runs > maxitr){
    stop("runs should not exceed maximum number of iterations", "\n")
  }
  
  population <- init_pop
  p <- 0.2
  i <- 1
  prev <- 0
  while(i <= maxitr){
    # fit <- foreach(i = 1:nrow(population), .combine = "c", .export= c("fitness", "Demand", "vehicle_capacity", "DistMat")) %dopar% {
    #   fitness(population[i, ], route = FALSE, Algo = "GA")
    # }
    fit <- c()
    edge_dist <- c()
    for(j in 1:nrow(population)){
      if(is.null(fitness)){
        temp <- fitness(population[j, ], vc, demand, DMat, route = TRUE)
      }
      fit <- c(fit, temp$fit)
      if(j == 1){
        edge_dist <- rbind(edge_dist, temp$edge_dist)
      }else if(ncol(edge_dist) < length(temp$edge_dist)){
        excess <- matrix(0, nrow = nrow(edge_dist), ncol = length(temp$edge_dist) - ncol(edge_dist))
        edge_dist <- rbind(cbind(excess, edge_dist), temp$edge_dist)
      }else if(ncol(edge_dist) > length(temp$edge_dist)){
        edge_dist <- rbind(edge_dist, c(rep(0, ncol(edge_dist) - length(temp$edge_dist)), temp$edge_dist))
      }else{
        edge_dist <- rbind(edge_dist, temp$edge_dist)
      }
    }
    cat("Iter - ",i,": Mean - ", mean(fit), " | Best - ", max(fit), "\n")
    best_sol <- population[which.max(fit), ]
    selected_ind <- selection(fit, edge_dist, pc = 0.6, p)
    # selected_ind <- selection(fit)
    worst_sol <- which.min(fit[selected_ind])[1]
    selected_ind <- selected_ind[-worst_sol]
    selected_ind <- sample(selected_ind)
    children <- foreach(j = seq(1, length(selected_ind), by = 2), .combine = "rbind", .export = "crossover") %dopar% {
      if(runif(1) <= cp){
        parents <- population[selected_ind[c(j, j+1)], ]
        crossover(parents)
      }else{
        population[selected_ind[c(j, j+1)], ]
      }
    }
    muted_pop <- foreach(j = 1:nrow(children), .combine = "rbind", .export = "mutation") %dopar% {
      if(runif(1) <= mp){
        mutation(children[j, ])
      }else{
        children[j, ]
      }
    }
    population <- rbind(best_sol, muted_pop)

    if(i == 1) { prev <- max(fit) }

    if(i %% runs == 0){
      if(max(fit) == prev){
        i <- maxitr
      }else{
        prev <- max(fit)
      }
    }
    i <- i + 1
  }
  return(list(pop = population, fit = fit))
}
