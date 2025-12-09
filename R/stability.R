# stability.R
#' Vérifie la condition de stabilité du schéma explicite
#' D * dt / dx^2 < 1/2
#' @return list(ok = TRUE/FALSE, r = value, dt_max = valeur recommandée)
#' @export
check_stability <- function(D, dx, dt) {
  r <- D * dt / (dx^2)
  dt_max <- 0.5 * dx^2 / D
  ok <- (r < 0.5)
  return(list(ok = ok, r = r, dt_max = dt_max))
}
