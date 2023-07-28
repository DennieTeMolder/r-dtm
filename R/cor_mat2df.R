##' @export
cor_mat2df <- function(cor_mat, use_names = FALSE) {
  stopifnot(is.matrix(cor_mat), nrow(cor_mat) == ncol(cor_mat))
  arr_ind <- which(lower.tri(cor_mat), arr.ind = TRUE)
  df <- tibble::tibble(row = arr_ind[, 1], col = arr_ind[, 2], value = cor_mat[arr_ind])
  if (use_names) {
    df$row <- rownames(cor_mat)[df$row]
    df$col <- colnames(cor_mat)[df$col]
  }
  df
}
