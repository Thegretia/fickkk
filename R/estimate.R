# estimate.R
#' Estimation du coefficient D à partir d'un tableau d'évolution (matrice temps x espace)
#' @param evolution matrix (nt x nx)
#' @param x numeric positions
#' @param times numeric vecteur temps (longueur = nrow(evolution))
#' @return list(D_est = ..., lm = object, var_vs_time = data.frame)
#' @export
diffusion_coefficient_estimate <- function(evolution, x, times) {
  if (!is.matrix(evolution)) stop("evolution doit être une matrice (temps x espace)")
  if (length(x) != ncol(evolution)) stop("x doit avoir longueur = ncol(evolution)")
  if (length(times) != nrow(evolution)) stop("times doit avoir longueur = nrow(evolution)")

  var_t <- numeric(length(times))
  mean_t <- numeric(length(times))

  for (i in seq_len(nrow(evolution))) {
    C <- evolution[i, ]
    # normaliser C pour qu'elle agisse comme densité (si somme > 0)
    if (sum(C) <= 0) {
      var_t[i] <- NA
      mean_t[i] <- NA
    } else {
      p <- C / sum(C)
      mean_t[i] <- sum(p * x)
      var_t[i] <- sum(p * (x - mean_t[i])^2)
    }
  }

  df <- data.frame(time = times, var = var_t)
  df <- df[!is.na(df$var), ]
  if (nrow(df) < 2) stop("Pas assez de points valides pour estimer D")

  fit <- lm(var ~ time, data = df)
  slope <- coef(fit)[2]
  D_est <- slope / 2

  return(list(D_est = D_est, lm = fit, var_vs_time = df))
}


