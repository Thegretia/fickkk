# flux.R

#' Compute diffusive flux using Fick's First Law
#'
#' Implements the biomedical form of Fick's First Law:
#'
#' \[
#'     J = - D \cdot S \cdot \frac{dC}{dx}
#' \]
#'
#' where:
#' - **D** : diffusion coefficient (e.g., cm²/s or m²/s)
#' - **S** : diffusion surface area
#' - **dC/dx** : concentration gradient across the membrane
#'
#' @param D numeric. Diffusion coefficient (must be ≥ 0).
#' @param dC numeric. Concentration difference (C2 - C1).
#' @param dx numeric. Distance or membrane thickness.
#'   Can be a scalar or vector. Must be > 0.
#' @param S numeric. Diffusion surface area. Must be > 0.
#'
#' @return numeric vector. Diffusive flux J (e.g., mg/s).
#' @export
ficks_law_flux <- function(D, dC, dx = 1, S = 1) {

  # --- Basic input validation ---
  if (!is.numeric(D) || length(D) != 1 || D < 0) {
    stop("D must be a single numeric value >= 0.")
  }

  if (!is.numeric(dC)) {
    stop("dC must be numeric.")
  }

  if (!is.numeric(dx) || any(dx <= 0)) {
    stop("dx must be numeric and strictly positive.")
  }

  if (!is.numeric(S) || S <= 0) {
    stop("S must be a strictly positive numeric value.")
  }

  # --- Allow dx to be scalar or vector ---
  # If scalar, replicate to match the length of dC
  if (length(dx) == 1) {
    dx <- rep(dx, length(dC))
  }

  if (length(dx) != length(dC)) {
    stop("dx must be either a scalar or the same length as dC.")
  }

  # --- Fick's First Law ---
  J <- - D * (dC / dx) * S

  return(J)
}


