# plot.R
#' Visualiser concentration (profil ou évolution)
#'
#' @param C numeric vecteur (espace) ou matrice (temps x espace)
#' @param x numeric positions (si NULL, 1:ncol(C) pour matrice, 1:length(C) pour vecteur)
#' @param times numeric vecteur de temps (optionnel, longueur = nrow(C))
#' @param main title
#' @param col palette ou couleur
#' @export
plot_concentration <- function(C, x = NULL, times = NULL, main = "Concentration", col = NULL) {
  if (is.matrix(C)) {
    nt <- nrow(C); nx <- ncol(C)
    if (is.null(x)) x <- seq_len(nx)
    if (length(x) != nx) stop("x doit avoir la même longueur que les colonnes de C")
    if (is.null(col)) col <- rainbow(nt)
    matplot(x, t(C), type = "l", lty = 1, col = col, xlab = "x", ylab = "Concentration", main = main)
    if (!is.null(times)) legend("topright", legend = paste0("t=", times), col = col, lwd = 2, cex = 0.8)
  } else if (is.numeric(C)) {
    if (is.null(x)) x <- seq_along(C)
    plot(x, C, type = "l", col = ifelse(is.null(col), "blue", col), xlab = "x", ylab = "Concentration", main = main)
  } else stop("C doit être un vecteur numérique ou une matrice")
}
