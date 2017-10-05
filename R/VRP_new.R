
library("Rglpk")


VRP <- function(time_mat, no_of_nodes, no_of_vehicle, cap_of_vehicle, max_time_for_vehicle, demand_node, delivery_time_at_node){
  zero_vec_for_u_i_j <- rep(0, no_of_nodes)
  obj_fun <- rep(as.vector(t(time_mat)), no_of_vehicle)
  obj_fun <- append(obj_fun, zero_vec_for_u_i_j, after = length(obj_fun))
  
  constraint_matrix <- matrix(0, nrow = (no_of_nodes*no_of_nodes + no_of_nodes*no_of_vehicle - no_of_nodes + 4*no_of_vehicle ), ncol = (no_of_nodes*no_of_nodes*no_of_vehicle + no_of_nodes))
  i <- 1
  
  print("1")
  ## sum(l_ijk = 1) for all j= 2, ..., N
  for(a in 2:no_of_nodes){
    null_vec <- rep(0, 0)
    dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
    
    for(b in 1:no_of_nodes){
      dummy_mat[b, a] <- 1
    }
    
    for(c in 1:no_of_vehicle){
      null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
    }
    
    null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
    constraint_matrix[i, ] <- null_vec
    i <- i + 1
  }
  
  ## sum(l_ijk = 1) for all i= 2, ..., N
  
  for(a in 2:no_of_nodes){
    null_vec <- rep(0, 0)
    dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
    
    for(b in 1:no_of_nodes){
      dummy_mat[a, b] <- 1
    }
    
    for(c in 1:no_of_vehicle){
      null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
    }
    
    null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
    constraint_matrix[i, ] <- null_vec
    i <- i + 1
  }
  
  print("2")
  for(a in 1:no_of_nodes){
    for(b in 1:no_of_vehicle){
      null_vec <- rep(0, 0)
      
      for(c in 1:no_of_vehicle){
        
        if(b == c){
          dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
          
          for(d in 1:no_of_nodes){
            
            for(e in 1:no_of_nodes){
              
              if(d == a || e == a){
                if(d == a && e == a)
                  dummy_mat[d, e] <- 0
                else if(d == a)
                  dummy_mat[d, e] <- -1
                else
                  dummy_mat[d, e] <-  1
                
                
              }
              
            }
          }
          null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        }
        
        else{
          dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
          null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        }
      }
      
      null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
      constraint_matrix[i,] <- null_vec
      i <- i + 1
      
    }
  }
  
  print("3")
  for(a in 1:no_of_vehicle){
    null_vec <- rep(0, 0)
    
    for(b in 1:no_of_vehicle){
      if(a == b){
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        for(c in 1:no_of_nodes){
          for(d in 1:no_of_nodes){
            dummy_mat[c, d] <- demand_node[c]
          }
        }
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
      }
      else{
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
      }
      
    }
    null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
    constraint_matrix[i, ] <- null_vec
    i <- i + 1
  }
  
  print("4")
  for(a in 1:no_of_vehicle){
    null_vec <- rep(0, 0)
    for(b in 1:no_of_vehicle){

      if(a == b){
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        for(c in 1:no_of_nodes){
          for(d in 1:no_of_nodes){
            dummy_mat[c, d] <-  time_mat[c, d]
          }
        }
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
      }

      else{
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))

      }
    }
    null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
    constraint_matrix[i,] <- null_vec
    i <- i + 1

  }
  print("5")
  for(a in 1:no_of_vehicle){
    null_vec <- rep(0, 0)
    for(b in 1:no_of_vehicle){
      
      if(a == b){
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        for(c in 2:no_of_nodes)
          dummy_mat[1, c] <- 1
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
      }
      
      else{
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
      }
    }
    null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
    constraint_matrix[i,] <- null_vec
    i <- i + 1
  }
  
  for(a in 1:no_of_vehicle){
    null_vec <- rep(0, 0)
    for(b in 1:no_of_vehicle){
      
      if(a == b){
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        for(c in 2:no_of_nodes)
          dummy_mat[c, 1] <- 1
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
      }
      
      else{
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
      }
    }
    null_vec <- append(null_vec, zero_vec_for_u_i_j, after = length(null_vec))
    constraint_matrix[i,] <- null_vec
    i <- i + 1
  }
  
  for(a in 2:no_of_nodes){
    for(b in 2:no_of_nodes){
      if(a!=b){
        null_vec <- rep(0, 0)
        dummy_mat <- matrix(0, nrow = no_of_nodes, ncol = no_of_nodes)
        dummy_mat[a, b] <- no_of_nodes
        for(c in 1:no_of_vehicle){
          null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
          
        }
        null_new_vec_use <- rep(0, n)
        null_new_vec_use[a] <- 1
        null_new_vec_use[b] <- -1
        null_vec <- append(null_vec, null_new_vec_use, after = length(null_vec))
        constraint_matrix[i, ] <- null_vec
        i <- i + 1
      }
    }
  }
  
  ## sign vector
  
  dir <- rep("==", (no_of_nodes*no_of_nodes + no_of_nodes*no_of_vehicle - no_of_nodes + 3*no_of_vehicle))
  x <- 1
  
  for(y in 2:no_of_nodes){
    dir[x] <- "=="
    x <- x + 1
  }
  
  for(y in 2:no_of_nodes){
    dir[x] <- "=="
    x <- x + 1
  }
  
  for(y in 1:(no_of_nodes*no_of_vehicle)){
    dir[x] <- "=="
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    dir[x] <- "<="
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    dir[x] <- "<="
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    dir[x] <- "<="
    x <- x + 1
  }
  for(y in 1:no_of_vehicle){
    dir[x] <- "<="
    x <- x + 1
  }
  
  for(y in 1:(no_of_nodes*no_of_nodes - 3*no_of_nodes + 2)){
    dir[x] <- "<="
    x <- x + 1
  }
  
## RHS for inequality
  rhs <- rep(0, (no_of_nodes*no_of_nodes + no_of_nodes*no_of_vehicle - no_of_nodes + 3*no_of_vehicle))
  x <- 1
  for(y in 2:no_of_nodes){
    rhs[x] <- 1
    x <- x + 1
  }
  
  for(y in 2:no_of_nodes){
    rhs[x] <- 1
    x <- x + 1
  }
  
  for(y in 1:(no_of_nodes*no_of_vehicle)){
    rhs[x] <- 0
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    rhs[x] <- cap_of_vehicle[y]
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    rhs[x] <- max_time_for_vehicle[y]
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    rhs[x] <- 1
    x <- x + 1
  }
  
  for(y in 1:no_of_vehicle){
    rhs[x] <- 1
    x <- x + 1
  }
  
  for(y in 1:(no_of_nodes*no_of_nodes - 3*no_of_nodes + 2)){
    rhs[x] <- no_of_nodes - 1
    x <- x + 1
  }
  
  ## types of variables
  types <- rep("I", (no_of_nodes*no_of_nodes*no_of_vehicle + no_of_nodes))
  x <- 1
  
  for(y in 1:(no_of_nodes*no_of_nodes*no_of_vehicle)){
    types[x] <- "B"
    x <- x + 1
  }
  
  for(y in 1:no_of_nodes){
    types[x] <- "C"
    x <- x + 1
  }
  
  max <- FALSE

  
  result <- Rglpk_solve_LP(obj_fun, constraint_matrix, dir, rhs, types = types, max = max)
  print("2")
  
  
  
  
  new_result <- matrix(result$solution[1:(no_of_nodes*no_of_nodes*no_of_vehicle)],  ncol = no_of_nodes, byrow = TRUE)
b <- 0
new_result_2 <- result$solution[1:(no_of_nodes*no_of_nodes*no_of_vehicle)]
for(a in 1:length(new_result_2)){
  if(new_result_2[a] == 1)
    b <-  b+1
}
print(b)
  # for(a in 1:no_of_nodes*no_of_vehicle){
  #   for(b in 1:no_of_nodes){
  #     if(new_result[a, b] == 1){
  #       c <- c(a,b)
  #       print(c)
  #     }
  #   }
  # }
  
print(new_result)
  

  # print(obj_fun)
  # print(types)
  # print(dir)
  # print(rhs)
  # print(constraint_matrix)

  return(result)
  
}


n <- 15
m <- 1
dist_mat <- read.csv("dist_new.csv", header = FALSE)
demand <- c(0,5,6,5,6,7,4,3,5,6,7,4,3,5,6)
vehicle_cap <- 100
max_time <- 5000
delivery_time <- c(0,4,4,4,5,6,4,4,2,3,4,5,5,6)

VRP(dist_mat,n,m, vehicle_cap, max_time, demand, delivery_time)



n <- 15
m <- 3
dist_mat <- read.csv("dist_new.csv", header = FALSE)
demand <- c(0,5,6,5,6,7,4,3,5,6,7,4,3,5,6)
vehicle_cap <- c(20, 30, 25)
max_time <- c(2000,3000, 1500)
delivery_time <- c(0,4,4,4,5,6,4,4,2,3,4,5,5,6)

VRP(dist_mat,n,m, vehicle_cap, max_time, demand, delivery_time)



n <- 3
m <- 1
dist_mat <- matrix(c(100,4,5,6,100,3,4,2,5,100), nrow = n, ncol = n)
demand <- c(0,4,5)
vehicle_cap <- 15
max_time <- 300
delivery_time <- c(0,3,6)
VRP(dist_mat,n,m, vehicle_cap, max_time, demand, delivery_time)

n <- 3
m <- 2
dist_mat <- matrix(c(100,4,5,6,100,3,4,2,100), nrow = n, byrow = TRUE)
demand <- c(0,6,7)
vehicle_cap <- c(7,7)
max_time <- c(30,35)
delivery_time <- c(0,3,6)
VRP(dist_mat,n,m, vehicle_cap, max_time, demand, delivery_time)


n <- 7
m <- 3
dist_mat <- read.csv("node_7.csv", header = FALSE)
demand <- c(0,4,5,6,3,7,8)
vehicle_cap <- c(15,14,12)
max_time <- c(1000,1000,1000)
delivery_time <- c(0,3,6,3,3,4,2)
VRP(dist_mat,n,m, vehicle_cap, max_time, demand, delivery_time)
