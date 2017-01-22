#' To compute total cost of the routes
#' @param result -- List of routes
#' @examples
#' data(An32k5locations)
#' locations <- An32k5locations
#' DMat <- DistMat(locations)
#' row.names(DMat) <- locations[, 1]
#' colnames(DMat) <- locations[, 1]
#' data(An32k5demand)
#' demand <- An32k5demand
#' Vehicle_Capacity <- 100
#' result <- CW_VRP(demand, DMat = DMat, Vehicle_Capacity = Vehicle_Capacity)
#' Total_Cost(result)
#' @export

Total_Cost <- function(result){
  cost <- 0
  for(i in 1:length(result)){
    cost <- cost + DMat[1, result[[i]][1]] + DMat[result[[i]][length(result[[i]])], 1]
    for(j in 1:(length(result[[i]]) - 1)){
      cost <- cost + DMat[result[[i]][j], result[[i]][j+1]]
    }
  }
  return(cost)
}