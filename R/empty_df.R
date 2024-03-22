# Generate an empty df with columns provided in ... + colnames
##' @export
empty_df <- function(..., colnames = NULL, nrow = 1, default = NA_real_) {
  if (!is.null(colnames))
    stopifnot(is.character(colnames))
  dots <- substitute_dots(...)
  dimnames <- list(rownames = NULL, colnames = c(dots, colnames))
  ncol <- length(dimnames[[2]])
  data.frame(matrix(data = default, nrow = nrow, ncol = ncol, dimnames = dimnames))
}
