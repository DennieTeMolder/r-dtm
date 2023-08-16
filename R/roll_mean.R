# Beter version of zoo::rollmean
##' @export
roll_mean <- function(x, k, silent = FALSE, ...) {
  if (!silent && k %% 2 == 0)
    warning("It is recommended to use on uneven size for 'k'.")
  k <- k / 2
  slider::slide_dbl(x, mean, ..., .before = ceiling(k) - 1, .after = floor(k))
}
