##' @export
coord_trim_x <- function(xmax, xmin = 0, factor = 0.04) {
  xlim <- c(xmin, xmax) + c(1, -1) * (xmax - xmin) * factor
  ggplot2::coord_cartesian(xlim = xlim)
}
