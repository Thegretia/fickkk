# estimate.R

#' Estimate the diffusion coefficient D from a time-evolution matrix
#'
#' This function estimates the diffusion coefficient \(D\) using the fact that,
#' for a diffusion process, the variance of the concentration profile evolves
#' linearly with time:
#' \deqn{\mathrm{Var}(x,t) = 2Dt + \mathrm{constant}}
#' A linear regression of variance vs time is therefore used to estimate D.
#'
#' @param evolution A numeric matrix of size (nt x nx) containing concentration
#' values over time. Each row corresponds to a time step.
#' @param x Numeric vector of spatial positions (length must equal ncol(evolution)).
#' @param times Numeric vector of time points (length must equal nrow(evolution)).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{D_est} — estimated diffusion coefficient
#'   \item \code{lm} — linear model object from \code{var ~ time}
#'   \item \code{var_vs_time} — data frame containing non-NA variance values
#' }
#' @export
#'
#' @examples
#' # Example with a simulated evolution matrix:
#' x <- seq(0, 1, length.out = 50)
#' times <- seq(0, 1, length.out = 10)
#' evolution <- matrix(runif(500), nrow = 10)
#' diffusion_coefficient_estimate(evolution, x, times)
diffusion_coefficient_estimate <- function(evolution, x, times) {

  # ---- Basic argument validation ----
  if (!is.matrix(evolution)) {
    stop("`evolution` must be a matrix (time x space).")
  }

  if (length(x) != ncol(evolution)) {
    stop("`x` must have length equal to ncol(evolution).")
  }

  if (length(times) != nrow(evolution)) {
    stop("`times` must have length equal to nrow(evolution).")
  }

  nt <- nrow(evolution)

  # Prepare vectors for variance and mean
  var_t  <- numeric(nt)
  mean_t <- numeric(nt)

  # ---- Compute the mean and variance for each time step ----
  for (i in seq_len(nt)) {
    C <- evolution[i, ]

    # If concentration sum is non-positive, variance cannot be computed
    if (sum(C) <= 0) {
      var_t[i]  <- NA
      mean_t[i] <- NA
    } else {
      # Normalize C into a probability distribution
      p <- C / sum(C)

      # Compute mean position
      mean_t[i] <- sum(p * x)

      # Compute variance
      var_t[i] <- sum(p * (x - mean_t[i])^2)
    }
  }

  # ---- Build the variance-vs-time data frame ----
  df <- data.frame(time = times, var = var_t)

  # Keep only valid rows
  df <- df[!is.na(df$var), ]

  if (nrow(df) < 2) {
    stop("Not enough valid points to estimate D.")
  }

  # ---- Linear regression: variance = 2D * t + constant ----
  fit <- stats::lm(var ~ time, data = df)

  slope <- coef(fit)[2]
  D_est <- slope / 2

  # ---- Return result ----
  return(list(
    D_est = D_est,
    lm = fit,
    var_vs_time = df
  ))
}
