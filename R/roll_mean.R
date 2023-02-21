# Pad out x with the first/last values such that rollmean returns length(x)
##' @export
roll_mean <- function(x, k) {
  x <- c(
    rep(dplyr::first(x), ceiling(k / 2) - 1),
    x,
    rep(dplyr::last(x), floor(k / 2))
  )
  zoo::rollmean(x, k = k, align = "center")
}
