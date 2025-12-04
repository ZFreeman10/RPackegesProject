#' @title ProjectNewImproved
#'
#' @param N Number of tickets to sell
#' @param gamma The overbooking probability tolerance
#' @param p The probability a passenger shows up
#'
#' @returns A list containing the number of tickets sold, probabilities, and plots.
#' @export
#'
#' @importFrom stats pbinom pnorm uniroot
#' @importFrom graphics par abline
#'
#' @examples
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
ntickets <- function(N, gamma, p) {
  nd <- NA
  for (n in N:(N + 50)) {
    if (1 - pbinom(N, n, p) <= gamma) {
      nd <- n
      break
    }
  }

  objective <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  nc <- uniroot(objective, lower = N, upper = N + 50)$root

  par(mfrow = c(2, 1))

  n_vals <- seq(N, N + 20, by = 1)
  obj_vals <- sapply(n_vals, function(n) 1 - gamma - pbinom(N, n, p))
  plot(n_vals, obj_vals, type = "b", pch = 19, col = "blue",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    nd, ") gamma=", gamma, " N=", N, " discrete"),
       xlab = "n", ylab = "Objective")
  abline(v = nd, col = "red", lwd = 2)
  abline(h = 0, col = "red")

  n_vals_cont <- seq(N, N + 20, by = 0.1)
  obj_vals_cont <- sapply(n_vals_cont, objective)
  plot(n_vals_cont, obj_vals_cont, type = "l", col = "black",
       main = paste("Objective Vs n to find optimal tickets sold\n(",
                    round(nc, 6), ") gamma=", gamma, " N=", N, " continuous"),
       xlab = "n", ylab = "Objective")
  abline(v = nc, col = "blue", lwd = 2)
  abline(h = 0, col = "blue")

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
