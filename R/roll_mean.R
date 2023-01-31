# Pad out x with the first/last values such that rollmean returns length(x)
##' @export
roll_mean <- function(x, k) {
  x <- c(
    rep(utils::head(x, n = 1), ceiling(k / 2) - 1),
    x,
    rep(utils::tail(x, n = 1), floor(k / 2))
  )
  zoo::rollmean(x, k = k, align = "center")
}
