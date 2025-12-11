# plot.R

#' Plot concentration profile or time evolution
#'
#' This function visualizes a concentration either as:
#' - a spatial profile (numeric vector), or
#' - a time evolution (matrix with rows = time and columns = space).
#'
#' @param C A numeric vector (spatial profile) or numeric matrix (time × space).
#' @param x Optional numeric positions.
#'   If NULL: uses 1:length(C) for a vector or 1:ncol(C) for a matrix.
#' @param times Optional numeric vector of time values (length must equal nrow(C)).
#' @param main Plot title.
#' @param col Colors for curves. If NULL, uses \code{rainbow()} for matrices and blue for vectors.
#'
#' @return Produces a plot; returns NULL invisibly.
#' @export
plot_concentration <- function(C, x = NULL, times = NULL, main = "Concentration", col = NULL) {

  # ------------------------------
  # Case 1 : C is a matrix
  # ------------------------------
  if (is.matrix(C)) {

    nt <- nrow(C)  # number of time steps
    nx <- ncol(C)  # number of spatial points

    # Generate x if missing
    if (is.null(x)) x <- seq_len(nx)

    # Check x length
    if (length(x) != nx) {
      stop("Length of 'x' must match the number of columns of matrix C.")
    }

    # Default color palette
    if (is.null(col)) col <- rainbow(nt)

    # Plot each time row as a curve
    matplot(
      x, t(C), type = "l", lty = 1, col = col,
      xlab = "x", ylab = "Concentration", main = main
    )

    # Add legend if times are provided
    if (!is.null(times)) {
      if (length(times) != nt) {
        stop("Length of 'times' must match the number of rows of matrix C.")
      }
      legend(
        "topright", legend = paste0("t = ", times),
        col = col, lwd = 2, cex = 0.8
      )
    }

    # ------------------------------
    # Case 2 : C is a numeric vector
    # ------------------------------
  } else if (is.numeric(C)) {

    # Generate x if missing
    if (is.null(x)) x <- seq_along(C)

    # Default color
    if (is.null(col)) col <- "blue"

    # Simple line plot for spatial profile
    plot(
      x, C, type = "l", col = col,
      xlab = "x", ylab = "Concentration", main = main
    )

    # ------------------------------
    # Invalid type
    # ------------------------------
  } else {
    stop("'C' must be either a numeric vector or numeric matrix.")
  }

  invisible(NULL)
}
