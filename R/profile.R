# profile.R

#' Generate a linear concentration profile between two boundary values
#'
#' This function computes a linear concentration profile between two
#' concentrations (C1 and C2) defined at the minimum and maximum of the
#' spatial domain x. It can be used to represent diffusion steady-states
#' or boundary-driven gradients.
#'
#' @param C1 numeric. Concentration at the minimum position of x.
#' @param C2 numeric. Concentration at the maximum position of x.
#' @param x numeric. Vector of spatial positions (order does not matter).
#'
#' @return A numeric vector of concentrations corresponding to each value in x.
#' @export
#'
#' @examples
#' x <- seq(0, 10, length.out = 5)
#' concentration_profile(1, 3, x)
concentration_profile <- function(C1, C2, x) {

  # Input validation
  if (!is.numeric(C1) || !is.numeric(C2)) {
    stop("C1 and C2 must be numeric.")
  }

  if (!is.numeric(x) || length(x) < 2) {
    stop("x must be a numeric vector of length >= 2.")
  }

  # Extract domain boundaries
  x_min <- min(x)
  x_max <- max(x)

  # If all x values are equal, return a constant profile
  if (x_max == x_min) {
    return(rep(C1, length(x)))
  }

  # Linear interpolation formula:
  # C(x) = C1 + (C2 - C1) * ( (x - x_min) / (x_max - x_min) )
  Cx <- C1 + (C2 - C1) * ((x - x_min) / (x_max - x_min))

  return(Cx)
}
