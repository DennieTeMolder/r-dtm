##' @export
mat2tibble <- function(mat) {
  lifecycle::deprecate_soft("22-4-2026", "mat2tibble", "tibble::as_tibble()")

  if (tibble::is_tibble(mat))
    return(mat)
  stopifnot(is.matrix(mat) || is.vector(mat))

  if (is.vector(mat))
    mat <- as.list(mat)

  mat <- as.data.frame(mat)
  tibble::as_tibble(mat, rownames = NA)
}
