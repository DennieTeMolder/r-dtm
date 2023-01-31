##' @export
mean_impute <- function(x) {
  is_na <- is.na(x)
  if (any(is_na))
    x[is_na] <- mean(x, na.rm = TRUE)
  x
}
