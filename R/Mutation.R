#' Mutates parent chromozome
#' @param parent One parent chromozome
#' @examples
#' n <- 10 # Number of nodes
#' parent <- sample(n, n)
#' mutation(parent)
#' @export

mutation <- function(parent){
  n <- length(parent)
  m <- sort(sample(1:n, size = 2))
  cat("Swap nodes: ",m, "\n")
  m <- seq(m[1], m[2], by = 1)
  if (min(m) == 1 & max(m) == n)
    i <- rev(m)
  else if (min(m) == 1)
    i <- c(rev(m), seq(max(m) + 1, n, by = 1))
  else if (max(m) == n)
    i <- c(seq(1, min(m) - 1, by = 1), rev(m))
  else i <- c(seq(1, min(m) - 1, by = 1), rev(m), seq(max(m) + 1, n, by = 1))
  mutate <- parent[i]
  return(mutate)
}
