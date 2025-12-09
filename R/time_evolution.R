# time_evolution.R
#' Simulation temporelle simple de la 2ème loi de Fick (1D) - schéma explicite FTCS
#'
#' ∂C/∂t = D ∂²C/∂x²
#' schéma explicite : C_i^{n+1} = C_i^n + r * (C_{i+1}^n - 2C_i^n + C_{i-1}^n)
#' où r = D * dt / dx^2
#'
#' @param C0 numeric Vecteur concentrations initiales en chaque position
#' @param D numeric Coefficient de diffusion (>=0)
#' @param dx numeric Pas spatial
#' @param dt numeric Pas temporel
#' @param steps integer Nombre d'itérations temporelles (>=1)
#' @param bc list Conditions aux limites; par défaut "dirichlet" c(0,0) ou "neumann"
#' @return matrix (steps+1) x length(C0) : chaque ligne = état à un pas temporel
#' @export
fick_time_evolution <- function(C0, D, dx = 1, dt = 0.1, steps = 100, bc = list(type = "zeroFlux")) {
  if (!is.numeric(C0) || length(C0) < 3) stop("C0 vecteur numérique de longueur >= 3")
  if (!is.numeric(D) || D < 0) stop("D doit être numérique >= 0")
  if (!is.numeric(dx) || dx <= 0) stop("dx > 0")
  if (!is.numeric(dt) || dt <= 0) stop("dt > 0")
  if (!is.numeric(steps) || steps < 1) stop("steps >= 1")

  r <- D * dt / (dx^2)
  # avertissement de stabilité (condition explicite)
  if (r >= 0.5) {
    warning(sprintf("Condition de stabilité peut être violée : r = %.4f >= 0.5. Réduire dt ou augmenter dx", r))
  }

  n <- length(C0)
  # matrice résultats : ligne 1 = t=0
  res <- matrix(NA_real_, nrow = steps + 1, ncol = n)
  res[1, ] <- C0

  # boucle temporelle
  Ccur <- C0
  for (t in seq_len(steps)) {
    Cnext <- Ccur
    # points intérieurs
    for (i in 2:(n - 1)) {
      Cnext[i] <- Ccur[i] + r * (Ccur[i + 1] - 2 * Ccur[i] + Ccur[i - 1])
    }
    # conditions aux limites (par défaut no-flux / Neumann 0)
    if (is.list(bc) && bc$type == "zeroFlux") {
      # Neumann 0: gradient nul aux bords => miroir
      Cnext[1]     <- Ccur[1]     + r * (Ccur[2]     - 2 * Ccur[1]     + Ccur[2])
      Cnext[n]     <- Ccur[n]     + r * (Ccur[n - 1] - 2 * Ccur[n]     + Ccur[n - 1])
    } else if (is.list(bc) && bc$type == "dirichlet") {
      # valeurs fixes aux bords : bc$left, bc$right
      left <- ifelse(is.null(bc$left), Ccur[1], bc$left)
      right <- ifelse(is.null(bc$right), Ccur[n], bc$right)
      Cnext[1] <- left
      Cnext[n] <- right
    } else {
      # fallback: zeroFlux
      Cnext[1]     <- Ccur[1]     + r * (Ccur[2]     - 2 * Ccur[1]     + Ccur[2])
      Cnext[n]     <- Ccur[n]     + r * (Ccur[n - 1] - 2 * Ccur[n]     + Ccur[n - 1])
    }

    res[t + 1, ] <- Cnext
    Ccur <- Cnext
  }

  return(res)
}
