# Fill NA/NaN values in X with the lowest adjacent non-NA/NaN value, see tidyr::fill()
##' @export
##' @importFrom rlang .data
fill_min <- function(x, lower_bound = 0L) {
  df <- data.frame(up = x, down = x)
  df <- tidyr::fill(df, .data$up, .direction = "up")
  df <- tidyr::fill(df, .data$down, .direction = "down")
  x <- pmin(df$up, df$down, na.rm = TRUE)
  if (!is.null(lower_bound))
    x[is.na(x)] <- lower_bound
  x
}
