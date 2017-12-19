library("Rglpk")


## Simple mixed integer linear program.
## maximize: 3 x_1 + 1 x_2 + 3 x_3
## subject to: -1 x_1 + 2 x_2 + x_3 <= 4
## 4 x_2 - 3 x_3 <= 2
## x_1 - 3 x_2 + 2 x_3 <= 3
## x_1, x_3 are non-negative integers
## x_2 is a non-negative real number
obj <- c(3, 1, 3)
mat <- matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(4, 2, 3)
types <- c("I", "C", "I")
max <- TRUE
Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)

# m - no of vehicle
# n - no of cities

VRP <- function(dist_mat, demand, vehicle_cap, n, m){
  
## min sigma (d_ij * x_ij^k ) + sigma ()
zero_vec_for_y_i_k <- rep(0, n*m)
obj <- rep(as.vector(t(dist_mat)), m)
obj <- append(obj, zero_vec_for_y_i_k, after = length(obj))

constraint_matrix <- matrix(0, nrow = (2*m + (n+1)*m + n*m + n), ncol = ((n+1)*(n+1)*m + n*m + n))

new_vec_u_i_u_j <- rep(0,n)
zero_vec_full <- rep(0, (n+1)*(n+1))
one_vec <- rep(1, n+1)
zero_vec_without_one <- rep(0, n*(n+1))
zero_vec_with_one <- append(one_vec, zero_vec_without_one, after = length(one_vec))
zero_vec_with_one[1] <- 0

for(i in 1:m){
  null_vec <- rep(0, 0)
  for(j in 1:m){
    if(i == j){
      null_vec <- append(null_vec, zero_vec_with_one, after = length(null_vec))
    }
    else
      null_vec <- append(null_vec, zero_vec_full, after = length(null_vec))
  }
  null_vec <- append(null_vec, zero_vec_for_y_i_k, after = length(null_vec))
  null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
  constraint_matrix[i,] <- null_vec
}

# w, x, y, z
i <- m + 1

for(w in 1:(n+1)){
  for(x in 1:m){
    
    null_vec <- rep(0,0)
    
    for(y in 1:m){
      
      if(x == y){
        dummy_mat <- matrix(0, nrow=(n+1), ncol = n+1)
        
        for(a in 1:(n+1)){
          for(b in 1:(n+1)){
            
            if((a == w || b == w) ){
              
              if(a==w && b==w){
                dummy_mat[a, b] <- 0
              }
              else if(b == w)
                dummy_mat[a, b] <- 1
              else
                dummy_mat[a, b] <- -1
              
            }
            
          }
        }
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
      }
      
      else{
        null_vec <- append(null_vec, zero_vec_full, after = length(null_vec))
        
      }
      
    }
    
    null_vec <- append(null_vec, zero_vec_for_y_i_k, after = length(null_vec))
    null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
    constraint_matrix[i,] <- null_vec
    i <- i + 1
    
  }
}


for(x in  1:n){
  
  null_vec <- rep(0,0)
  for(w in 1:m)
    null_vec <- append(null_vec, zero_vec_full, after = length(null_vec))
  
  
  dummy_vec <- rep(0, n)
  dummy_vec[x] <- 1
  
  for(z in 1:m)
    null_vec <- append(null_vec, dummy_vec, after = length(null_vec))
  null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
  
  constraint_matrix[i,] <- null_vec
  i <- i+1
}

for(x in 1:m){
  
  null_vec <- rep(0,0)
  
  for(w in 1:m){
    null_vec <- append(null_vec, zero_vec_full, after = length(null_vec))
  }
  
  null_new <- rep(0, n)
  for(y in 1:m){
    if(x != y)
      null_vec <- append(null_vec, null_new, after = length(null_vec))
    else{
      null_vec <- append(null_vec, demand, after = length(null_vec))
    }
  }
  null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
  
  constraint_matrix[i,] <- null_vec
  i <- i+1
  
}


for(w in 2:(n+1)){
  for(x in 1:m){
    
    null_vec <- rep(0,0)
    
    dummy_mat <- matrix(0, nrow = n+1, ncol = n+1)
    for(a in 1:(n+1)){
      for(b in 1:(n+1)){
        if(b == w)
          dummy_mat[a, b] <- -1
      }
    }
    
    dummy_vec <- rep(0, n)
    dummy_vec[w-1] <- 1
    dummy_new <- rep(0,n)
    
    for(a in 1:m){
      if(a == x)
        null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
      else
        null_vec <- append(null_vec, zero_vec_full, after = length(null_vec))
    }
    
    for(a in 1:m){
      
      if(a == x)
        null_vec <- append(null_vec, dummy_vec, after = length(null_vec))
      else
        null_vec <- append(null_vec,dummy_new, after = length(null_vec))
      
    }
    null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
    
    constraint_matrix[i,] <- null_vec
    i <- i+1
    
  }
  
}


# ## x_i_j + x_j_i <= 1
# 
# for(a in 2:(n+1)){
#   for(b in 2:(n+1)){
#     for(c in 1:m){
#       
#       null_vec <- rep(0,0)
#       dummy_mat <- matrix(0, nrow = n+1, ncol = n+1)
#       if(a !=b){
#         dummy_mat[a,b] <- 1
#         dummy_mat[b,a] <- 1
#       }
#       
#       else{
#         dummy_mat[a, b] <- 2
#       }
#       
#       for(d in 1:m){
#         if(d == c)
#           null_vec <- append(null_vec, as.vector(t(dummy_mat)), after = length(null_vec))
#         else
#           null_vec <- append(null_vec, zero_vec_full, after = length(null_vec))
#       }
#       
#       null_vec <- append(null_vec, zero_vec_for_y_i_k, after = length(null_vec))
#       null_vec <- append(null_vec, new_vec_u_i_u_j, after = length(null_vec))
#       
#       constraint_matrix[i,] <- null_vec
#       i <- i+1
#     }
#   }
# }
# 

#sum(x_ij for all i) = 1




## sign vector

dir <- rep("==", (2*m + (n+1)*m + n*m + n ))
x <- 1

for(y in 1:(m + ( n + 1 ) * m + n)){
  dir[x] <- "=="
  x <- x + 1
}

for(y in 1:(m + n*m)){
  dir[x] <- "<="
  x <- x + 1
}

# for(y in 1:(n)*(n)*m){
#   dir[x] <- "<="
#   x <- x + 1
# }


## RHS for inequality

rhs <- rep(0, (2*m + (n+1)*m + n*m + n )
x <- 1

for(y in 1:m){
  rhs[x] <- 1
  x <- x + 1
}
  
for(y in 1:((n + 1) * m)){
  rhs[x] <- 0
  x <- x + 1
}

for(y in 1:n){
  rhs[x] <- 1
  x <- x + 1
}

for(y in 1:m){
  rhs[x] <- vehicle_cap[y]
  x <- x + 1
}

# for(y in 1:(n)*(n)*m){
#   rhs[x] <- 2
#   x <- x + 1
# }

## types of variables
types <- rep("I",((n+1)*(n+1)*m + n*m + n))
x <- 1

for(y in 1:((n + 1)*(n + 1)*m )){
  types[x] <- "B"
  x <- x + 1
}

for(y in 1:(n*m)){
  types[x] <- "C"
  x <- x + 1
}

for(y in 1:(n)){
  types[x] <- "C"
  x <- x + 1
}

max <- FALSE
print("1")
result <- Rglpk_solve_LP(obj, constraint_matrix, dir, rhs, types = types, max = max)
print("2")
# result_new <- result$solution[1:(n+1)*(n+1)]
# cordinate <- matrix(result_new, nrow = (n+1), ncol = (n+1), byrow = TRUE)
# 
# for(a in 1:(n+1)){
#   for(b in 1:(n+1)){
#     if(cordinate[a,b] == 1){
#       f <- c(a,b)
#       print(f)
#     }
# 
#   }
# }

print(result)
}

n <- 47
m <- 1
dist_mat_1 <- read.csv("dist.csv", header = FALSE)

demand_1 <- c(10,8,12,34,12,34,45,53,45,23,23,12,34,23,10,8,12,34,12,34,45,53,45,23,23,12,34,23,10,8,12,34,12,34,45,53,45,23,23,12,34,23,34,2,45,43,54)
vehicle_cap_1 <- c(1000000)

VRP(dist_mat_1, demand_1 , vehicle_cap_1, n, m)

n <- 2
m <- 1
dist_mat <- matrix(c(110,110,110,110,5,6,110,6,4))
demand <- c(5,6)
vehicle_cap <- 4
VRP(dist_mat, demand, vehicle_cap, n, m)
