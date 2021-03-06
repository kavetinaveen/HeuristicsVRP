#' Performs Clarke-Wright Savings algorithm in Parallel
#' @param Sort_Edge -- Edges sorted in a decreasing order of their savings
#' @param nnodes -- Number of customer nodes
#' @export

CW_Parallel_VRP <- function(Sort_Edge, nnodes, demand, Vehicle_Capacity, routes = list(), Served_nodes = c(), Filled_Routes = c(), logfile = TRUE, Constraints = c("Capacity")){
  # If all the nodes are serviced then return routes
  if(logfile){
    if(is.null(Served_nodes)){
      if(file.exists("Results.txt")) file.remove("Results.txt")
    }
    out <- capture.output(routes)
    cat("Route: ", out, file = "Results.txt", sep = "\n", append = TRUE)
  }
  
  if(length(Served_nodes) == nnodes | nrow(Sort_Edge) == 0){
    cat("Number of nodes serviced := ", length(Served_nodes), "\n")
    return(routes)
  }
  
  CommonNode <- intersect(c(Sort_Edge[1, 1], Sort_Edge[1, 2]), Served_nodes)
  NewNode <- setdiff(c(Sort_Edge[1, 1], Sort_Edge[1, 2]), CommonNode)
  nCommonNodes <- length(CommonNode)
  
  if(nCommonNodes == 0){ # If there is no common node between new edge and existing routes then create a new route
    
    routes[[length(routes) + 1]] <- c(Sort_Edge[1, 1], Sort_Edge[1, 2])
    Served_nodes <- unique(c(Served_nodes, c(Sort_Edge[1, 1], Sort_Edge[1, 2])))
    return(CW_Parallel_VRP(Sort_Edge[-1, ], nnodes, demand, Vehicle_Capacity, routes = routes, Served_nodes = Served_nodes, Constraints = Constraints))
    
  }else if(nCommonNodes == 1){ # If there is exactly one common node between new edge and existing routes then check the constraints validation and connect the edges
    route_id <- 0
    # Find route id in which common node exists 
    for(i in 1:length(routes)){
      if(CommonNode %in% routes[[i]]) route_id <- i
    }
    # If route is already filled then continue with the other edges
    if(route_id %in% Filled_Routes){
      return(CW_Parallel_VRP(Sort_Edge[-1, ], nnodes, demand, Vehicle_Capacity, routes = routes, Served_nodes = Served_nodes, Constraints = Constraints))
    }
    # Check whether common node is interior or not? And if it is interior delete the first edge and continue 
    if(isInterior(routes[[route_id]], CommonNode)){
      return(CW_Parallel_VRP(Sort_Edge[-1, ], nnodes, demand, Vehicle_Capacity, routes = routes, Served_nodes = Served_nodes, Constraints = Constraints))
    }
    
    CC <- CheckConstraints(routes[[route_id]], CommonNode, NewNode, demand, Vehicle_Capacity, Constraints)
    if(CC$Filled) Filled_Routes <- c(Filled_Routes, route_id)
    
    # Check whether including new node to existing route satisfying constraints or not? If it satisfies include new node into route, otherwise delete the first edge and continue
    if(CC$Satisfied){
      routes[[route_id]] <- Connect_Edges(route = routes[[route_id]], common_node = CommonNode, new_node = NewNode)
      Served_nodes <- c(Served_nodes, NewNode)
      return(CW_Parallel_VRP(Sort_Edge[-1, ], nnodes, demand, Vehicle_Capacity, routes = routes, Served_nodes = Served_nodes, Constraints = Constraints))
    }else{
      return(CW_Parallel_VRP(Sort_Edge[-1, ], nnodes, demand, Vehicle_Capacity, routes = routes, Served_nodes = Served_nodes, Constraints = Constraints))
    }
  }else if(nCommonNodes == 2){ # If both the nodes are common then remove the edge from the Sort_Edge
    return(CW_Parallel_VRP(Sort_Edge[-1, ], nnodes, demand, Vehicle_Capacity, routes = routes, Served_nodes = Served_nodes, Constraints = Constraints))
  }
}
