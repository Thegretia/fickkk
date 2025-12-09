# flux.R
#' Calcul du flux diffusif selon la 1ère loi de Fick (biomedicale)
#'
#' J = - D * S * (dC / dx)
#'
#' @param D numeric Coefficient de diffusion (cm^2/s ou m^2/s)
#' @param dC numeric Différence de concentration (C2 - C1)
#' @param dx numeric Distance (épaisseur de la membrane en cm)
#' @param S numeric surface de diffusion (cm^2)
#' @return numeric Flux diffusif J (mg/s)
#' @export
ficks_law_flux <- function(D, dC, dx = 1, S = 1) {
  if (!is.numeric(D) || length(D) != 1 || D < 0) stop("D doit être un nombre >= 0")
  if (!is.numeric(dC)) stop("dC doit être numérique")
  if (!is.numeric(dx) || any(dx <= 0)) stop("dx doit être numérique strictement positif")
  if (S <= 0) stop("S doit être > 0")
  # permettre dx scalaire ou vecteur
  if (length(dx) == 1) dx <- rep(dx, length(dC))
  if (length(dx) != length(dC)) stop("dx doit être de même longueur que dC ou scalaire")

  J <- - D * (dC / dx) * S
  return(J)
}



