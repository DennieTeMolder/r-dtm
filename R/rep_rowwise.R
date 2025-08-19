# Repeat a vector rowwise to create a matrix, contrary to default behaviour
##' @export
rep_rowwise <- function(x, nrow = NULL, rownames = NULL) {
  stopifnot(is.vector(x) || is.matrix(x) && nrow(x) == 1L)
  stopifnot(!is.null(nrow) || !is.null(rownames))
  if (is.null(nrow)) {
    nrow <- length(rownames)
  } else {
    stopifnot(is.numeric(nrow), length(nrow) == 1L)
    if (!is.null(rownames))
      stopifnot(nrow == length(rownames))
  }

  matrix(
    rep(x, each = nrow),
    nrow = nrow,
    ncol = length(x),
    dimnames = list(rownames, if (is.matrix(x)) colnames(x) else names(x))
  )
}
