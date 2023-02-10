##' @export
kinship2dist <- function(mat) {
  stopifnot(is.matrix(mat))
  as.dist((mat - max(mat)) * -1)
}
