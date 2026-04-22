# Convert kinship matrix MAT to a distance matrix, see kinship()
##' @export
kinship2dist <- function(mat) {
  stopifnot(is.matrix(mat))
  stats::as.dist((mat - max(mat)) * -1)
}
