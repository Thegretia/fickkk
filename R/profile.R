# profile.R
#' Génère un profil de concentration linéaire entre C1 et C2 dans un tissu
#'
#' @param C1 numeric Concentration au point x_min
#' @param C2 numeric Concentration au point x_max
#' @param x numeric Vecteur des positions (ordre quelconque)
#' @return numeric Vecteur des concentrations C(x)
#' @export
concentration_profile <- function(C1, C2, x) {
  if (!is.numeric(C1) || !is.numeric(C2)) stop("C1 et C2 doivent être numériques")
  if (!is.numeric(x) || length(x) < 2) stop("x doit être un vecteur numérique de longueur >= 2")
  x0 <- min(x); x1 <- max(x)
  if (x1 == x0) return(rep(C1, length(x)))
  # interpolation linéaire entre (x0,C1) et (x1,C2)
  Cx <- C1 + (C2 - C1) * ( (x - x0) / (x1 - x0) )
  return(Cx)
}

