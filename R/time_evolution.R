# time_evolution.R

#' Time evolution of the 1D diffusion equation (Fick's second law)
#'
#' This function solves the 1D diffusion equation using the explicit FTCS scheme:
#'
#' \deqn{\frac{\partial C}{\partial t} = D \frac{\partial^2 C}{\partial x^2}}
#'
#' Explicit finite-difference update:
#'
#' \deqn{C_i^{n+1} = C_i^n + r \left( C_{i+1}^n - 2C_i^n + C_{i-1}^n \right)}
#'
#' where \eqn{r = D \, dt / dx^2}.
#'
#' The method is stable only if \eqn{r < 0.5}.
#'
#' @param C0 Numeric vector of initial concentrations at each spatial position.
#' @param D Numeric diffusion coefficient (must be >= 0).
#' @param dx Spatial step (positive).
#' @param dt Time step (positive).
#' @param steps Number of time iterations (integer >= 1).
#' @param bc Boundary-condition list.
#'   Supported types:
#'   * `"zeroFlux"` (Neumann 0): zero gradient at boundaries
#'   * `"dirichlet"`: fixed values `left` and `right`
#'
#' @return A matrix of size `(steps + 1) x length(C0)` where each row is the
#' concentration vector at a given time.
#'
#' @export
fick_time_evolution <- function(
    C0,
    D,
    dx = 1,
    dt = 0.1,
    steps = 100,
    bc = list(type = "zeroFlux")
) {
  # ---- Argument validation ----
  if (!is.numeric(C0) || length(C0) < 3) {
    stop("C0 must be a numeric vector of length >= 3.")
  }
  if (!is.numeric(D) || D < 0) stop("D must be a numeric value >= 0.")
  if (!is.numeric(dx) || dx <= 0) stop("dx must be a positive numeric value.")
  if (!is.numeric(dt) || dt <= 0) stop("dt must be a positive numeric value.")
  if (!is.numeric(steps) || steps < 1) stop("steps must be an integer >= 1.")

  n <- length(C0)

  # ---- Stability parameter ----
  r <- D * dt / dx^2
  if (r >= 0.5) {
    warning(sprintf(
      "Stability condition violated or close to violation: r = %.4f (must be < 0.5).",
      r
    ))
  }

  # ---- Output matrix ----
  res <- matrix(NA_real_, nrow = steps + 1, ncol = n)
  res[1, ] <- C0

  # ---- Temporal loop ----
  Ccur <- C0
  for (t in seq_len(steps)) {
    Cnext <- Ccur

    # ---- Interior points: standard FTCS update ----
    for (i in 2:(n - 1)) {
      Cnext[i] <- Ccur[i] + r * (Ccur[i + 1] - 2 * Ccur[i] + Ccur[i - 1])
    }

    # ---- Boundary conditions ----
    if (is.list(bc) && bc$type == "zeroFlux") {
      # Neumann 0 ⇒ mirrored neighbors (dC/dx = 0)
      Cnext[1] <- Ccur[1] + r * (Ccur[2] - 2 * Ccur[1] + Ccur[2])
      Cnext[n] <- Ccur[n] + r * (Ccur[n - 1] - 2 * Ccur[n] + Ccur[n - 1])

    } else if (is.list(bc) && bc$type == "dirichlet") {
      # Fixed boundary values
      left  <- ifelse(is.null(bc$left),  Ccur[1], bc$left)
      right <- ifelse(is.null(bc$right), Ccur[n], bc$right)
      Cnext[1] <- left
      Cnext[n] <- right

    } else {
      # Fallback: zero-flux
      Cnext[1] <- Ccur[1] + r * (Ccur[2] - 2 * Ccur[1] + Ccur[2])
      Cnext[n] <- Ccur[n] + r * (Ccur[n - 1] - 2 * Ccur[n] + Ccur[n - 1])
    }

    res[t + 1, ] <- Cnext
    Ccur <- Cnext
  }

  return(res)
}
