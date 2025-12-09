# simulate.R
#' Lancer une simulation complète (profil initial + évolution + flux)
#'
#' @param C1 numeric concentration gauche
#' @param C2 numeric concentration droite
#' @param x numeric vecteur positions
#' @param D numeric coeff de diffusion
#' @param dx numeric pas spatial (si NULL calculé depuis x)
#' @param dt numeric pas temporel
#' @param steps integer nb d'itérations
#' @param bc list conditions aux limites (passé à fick_time_evolution)
#' @return liste avec profils, flux, evolution (matrice) et le plot (NULL)
#' @export
simulate_diffusion <- function(C1 = 1, C2 = 0, dose = 1,x = seq(0,1,length.out = 101), D = 0.01,
                               dx = NULL, dt = 0.0005, steps = 200, bc = list(type = "dirichlet", left = C1, right = C2)) {
  if (is.null(dx)) dx <- mean(diff(x))
  # profil initial linéaire
  C0 <- concentration_profile(C1, C2, x) * dose
  # flux initial (approx dC/dx central simple) : dC = C[i+1]-C[i]
  dC <- diff(C0)
  # associer dx per intervalle (length n-1)
  J_inter <- ficks_law_flux(D = D, dC = dC, dx = dx)
  # évolution temporelle
  evolution <- fick_time_evolution(C0 = C0, D = D, dx = dx, dt = dt, steps = steps, bc = bc)
  # encapsuler
  res <- list(
    x = x,
    C0 = C0,
    flux_initial = c(J_inter, NA), # on ajoute NA pour la dernière position (ou aligner selon choix)
    evolution = evolution,
    params = list(D = D, dx = dx, dt = dt, steps = steps)
  )
  # tracer rapidement si demandé (ici on ne force pas popup)
  return(res)
}



