##' @export
##' @importFrom rlang .data
fill_min <- function(x) {
  df <- data.frame(up = x, down = x)
  df <- tidyr::fill(df, .data$up, .direction = "up")
  df <- tidyr::fill(df, .data$down, .direction = "down")
  x <- pmin(df$up, df$down, na.rm = TRUE)
  x[is.na(x)] <- 0
  x
}
