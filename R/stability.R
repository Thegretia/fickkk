# stability.R

#' Check the stability condition of the explicit FTCS scheme
#'
#' For the explicit finite-difference scheme applied to Fick's second law,
#' the stability requirement is:
#'
#' \[
#'     r = \frac{D \, dt}{dx^2} < \frac{1}{2}
#' \]
#'
#' This function computes the stability parameter `r`, evaluates whether the
#' condition is satisfied, and returns the recommended maximum time step.
#'
#' @param D numeric. Diffusion coefficient (must be > 0).
#' @param dx numeric. Spatial step size (must be > 0).
#' @param dt numeric. Time step size (must be > 0).
#'
#' @return list containing:
#' \describe{
#'   \item{ok}{logical. TRUE if the scheme is stable, FALSE otherwise.}
#'   \item{r}{numeric. The computed stability ratio \( r = D\,dt/dx^2 \).}
#'   \item{dt_max}{numeric. Maximum stable time step \( dt_{\max} = \frac{0.5 \, dx^2}{D} \).}
#' }
#'
#' @export
check_stability <- function(D, dx, dt) {

  # --- Input validation ---
  if (!is.numeric(D) || D <= 0) {
    stop("D must be a strictly positive numeric value.")
  }

  if (!is.numeric(dx) || dx <= 0) {
    stop("dx must be a strictly positive numeric value.")
  }

  if (!is.numeric(dt) || dt <= 0) {
    stop("dt must be a strictly positive numeric value.")
  }

  # --- Compute stability ratio ---
  r <- D * dt / (dx^2)

  # --- Maximum stable dt (from r < 0.5 ) ---
  dt_max <- 0.5 * dx^2 / D

  # --- Evaluate stability ---
  ok <- (r < 0.5)

  return(list(
    ok = ok,
    r = r,
    dt_max = dt_max
  ))
}
