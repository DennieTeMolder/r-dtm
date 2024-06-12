##' @export
mat2tibble <- function(mat) {
  if (tibble::is_tibble(mat))
    return(mat)
  stopifnot(is.matrix(mat) || is.vector(mat))

  if (is.vector(mat)) {
    mat <- matrix(mat, nrow = 1L, dimnames = list(NULL, names(mat)))
  }

  mat <- as.data.frame(mat)
  tibble::as_tibble(mat, rownames = NA)
}
