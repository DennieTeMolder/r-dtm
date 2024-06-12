##' @export
# Alternative to tidyr::replace_na() in which replacement can also be a vector
na_replace <- function(x, replacement) {
  stopifnot(length(replacement) == 1L || length(replacement) == length(x))
  is_na <- is.na(x)
  if (any(is_na)) {
    if (length(replacement) == 1L) {
      x[is_na] <- replacement
    } else {
      x[is_na] <- replacement[is_na]
    }
  }
  x
}
