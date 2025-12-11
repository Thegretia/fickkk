# simulate.R

#' Run a complete diffusion simulation (initial profile + evolution + flux)
#'
#' This function performs a full simulation of diffusion by:
#' 1. Generating an initial linear concentration profile between `C1` and `C2`
#' 2. Computing the initial flux using Fick's first law
#' 3. Simulating temporal evolution using Fick's second law
#'
#' @param C1 numeric. Concentration at the left boundary.
#' @param C2 numeric. Concentration at the right boundary.
#' @param dose numeric. Multiplicative factor applied to the initial profile.
#' @param x numeric. Vector of spatial positions.
#' @param D numeric. Diffusion coefficient.
#' @param dx numeric. Spatial step. If NULL, it is estimated from `x`.
#' @param dt numeric. Time step for the simulation.
#' @param steps integer. Number of time iterations.
#' @param bc list. Boundary conditions passed to `fick_time_evolution()`.
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{Spatial grid}
#'   \item{C0}{Initial concentration profile}
#'   \item{flux_initial}{Vector of initial flux values}
#'   \item{evolution}{Matrix of concentration over time}
#'   \item{params}{List of simulation parameters}
#' }
#' @export
#'
#' @examples
#' res <- simulate_diffusion(C1 = 1, C2 = 0, D = 0.01, steps = 50)
#' str(res)
simulate_diffusion <- function(
    C1 = 1,
    C2 = 0,
    dose = 1,
    x = seq(0, 1, length.out = 101),
    D = 0.01,
    dx = NULL,
    dt = 0.0005,
    steps = 200,
    bc = list(type = "dirichlet", left = C1, right = C2)) {

  # --- Basic input validation ---
  if (!is.numeric(C1) || !is.numeric(C2)) {
    stop("C1 and C2 must be numeric.")
  }

  if (!is.numeric(x) || length(x) < 2) {
    stop("x must be a numeric vector of length >= 2.")
  }

  if (!is.numeric(D) || D <= 0) {
    stop("D must be a positive numeric value.")
  }

  if (!is.numeric(dt) || dt <= 0) {
    stop("dt must be a positive number.")
  }

  if (!is.numeric(steps) || steps <= 0) {
    stop("steps must be a positive numeric value.")
  }

  # If dx is not given, compute it using mean spacing
  if (is.null(dx)) dx <- mean(diff(x))

  # --- Build the initial concentration profile ---
  C0 <- concentration_profile(C1, C2, x) * dose

  # --- Compute initial flux from Fick's First Law ---
  # dC[i] = C[i+1] - C[i] (central difference approximation)
  dC <- diff(C0)

  # Flux computed at midpoints of intervals
  J_intervals <- ficks_law_flux(D = D, dC = dC, dx = dx)

  # --- Compute time evolution using Fick's Second Law ---
  evolution <- fick_time_evolution(
    C0 = C0,
    D = D,
    dx = dx,
    dt = dt,
    steps = steps,
    bc = bc
  )

  # --- Encapsulate results ---
  res <- list(
    x = x,
    C0 = C0,
    flux_initial = c(J_intervals, NA),  # NA for alignment with x length
    evolution = evolution,
    params = list(D = D, dx = dx, dt = dt, steps = steps)
  )

  return(res)
}



