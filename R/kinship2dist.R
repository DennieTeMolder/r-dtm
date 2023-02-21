##' @export
kinship2dist <- function(mat) {
  stopifnot(is.matrix(mat))
  stats::as.dist((mat - max(mat)) * -1)
}
