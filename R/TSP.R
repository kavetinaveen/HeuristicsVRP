library(data.table)
library("Rglpk")
setwd("/Users/lnv0001/Documents/order_forecast/")
TSP <- function(dist_mat, n){
  
  new_vec_u_i_u_j <- rep(0,n)
  obj <- as.vector(t(dist_mat))
  obj <- append(obj,new_vec_u_i_u_j, after = length(obj) )
  
  constraint_matrix <- matrix(0, nrow = (n*n -n + 2), ncol =(n*n + n) )
  i <- 1
  
  for(a in 1:n){
    null_vec <- rep(0, 0)
    dummy_mat <- matrix(0,  nrow = n, ncol = n)
    for(b in 1:n)
      dummy_mat[b, a] <- 1
    null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
    null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
    constraint_matrix[i,] <- null_vec
    i <- i + 1
  }
  
  for(a in 1:n){
    null_vec <- rep(0, 0)
    dummy_mat <- matrix(0,  nrow = n, ncol = n)
    for(b in 1:n)
      dummy_mat[a, b] <- 1
    
    null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
    null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
    
    constraint_matrix[i,] <- null_vec
    i <- i + 1
  }
  
  ## subtour constraint
  for(a in 2:n){
    for(b in 2:n){
      
      if(a!=b){
        null_vec <- rep(0,0)
        
        dummy_mat <- matrix(0, nrow = n, ncol = n)
        dummy_mat[a, b] <- n
        
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
        
        null_new_vec <- rep(0, n)
        null_new_vec[a ] <- 1
        null_new_vec[b ] <- -1
        null_vec <- append(null_vec, null_new_vec, after = length(null_vec))
        constraint_matrix[i,] <- null_vec
        i <- i+1
      }
      
    }
  }
  
  ## sign
  dir <- rep("==", (n*n -n + 2))
  x <- 1
  
  for(y in 1:(2*n)){
    dir[x] <- "=="
    x <- x + 1
  }
  
  for(y in 1:(n*n -3*n + 2)){
    dir[x] <- "<="
    x <- x + 1
  }
  
  ## equality RHS
  rhs <- rep(0, (n*n -n + 2))
  x <- 1
  
  for(y in 1:(2*n)){
    rhs[x] <- 1
    x <- x + 1
  }
  
  
  for(y in 1:(n*n -3*n + 2)){
    rhs[x] <- n - 1
    x <- x + 1
  }
  
  ## types of variables
  types <- rep("I", (n*n + n))
  x <- 1
  
  for(y in 1:(n*n)){
    types[x] <- "B"
    x <- x + 1
  }
  
  for(y in 1:(n)){
    types[x] <- "C"
    x <- x + 1
  }
  
  max <- FALSE
  result <- Rglpk_solve_LP(obj, constraint_matrix, dir, rhs, types = types, max = max)
  
  result_new <- result$solution[1:(n*n)]
  cordinate <- matrix(result_new, nrow = n, ncol = n, byrow = TRUE)
  for(a in 1:n){
    for(b in 1:n){
      if(cordinate[a, b] == 1){
        c <- c(a, b)
        print(c)
      }
    }
  }
  
  print(result)
  print(constraint_matrix)
  
}


n <- 15
dist_mat_1 <- read.csv("dist_new.csv", header = FALSE)
initial_time <- Sys.time()
TSP(dist_mat_1, n)
print(Sys.time()-initial_time)

