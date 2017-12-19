#' Performs Order Crossover (OX) between two chromozomes
#' @param parents Selected parents
#' @examples
#' n <- 10 # Number of nodes
#' parents <- rbind(sample(n, n), sample(n, n))
#' crossover(parents)
#' @export

crossover <- function(parents){
  n <- ncol(parents)
  cxPoints <- sample(seq(2, n - 1), size = 2)
  cxPoints <- seq(min(cxPoints), max(cxPoints))
  cat("Crossover points: ", cxPoints, "\n")
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  children[, cxPoints] <- parents[, cxPoints]
  for (j in 1:2) {
    pos <- c((max(cxPoints) + 1):n, 1:(max(cxPoints)))
    val <- setdiff(parents[-j, pos], children[j, cxPoints])
    i <- intersect(pos, which(is.na(children[j, ])))
    children[j, i] <- val
  }
  return(children)
}
