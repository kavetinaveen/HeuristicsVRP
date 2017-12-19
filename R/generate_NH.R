#' Function to generate neighbor solution. It's an internal function to SA_VRP.
#' @param solution solution vector
#' @examples 
#' solution <- sample(10, 10)
#' generate_neighbor(solution)
#' @export

generate_neighbor <- function(solution){
  n <- length(solution)
  new_solution <- solution
  swap <- sample(n, 2)
  new_solution[swap[1]] <- solution[swap[2]]
  new_solution[swap[2]] <- solution[swap[1]]
  return(new_solution)
}

#' To compute acceptance probability
#' @param old_cost Old solution cost
#' @param new_cost New solution cost
#' @param Temp Temperature

acceptance_probability <- function(old_cost, new_cost, Temp){
  return(exp((old_cost - new_cost)/Temp))
}
