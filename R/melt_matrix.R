# Melt matrix MAT into a long-format dataframe with row/column indices or names
# based on USE_NAMES.
##' @export
melt_matrix <- function(mat, upper = TRUE, diag = upper, use_names = TRUE) {
  mat <- as.matrix(mat)

  rnames <- if (use_names) rownames(mat)
  if (is.null(rnames)) rnames <- seq_len(nrow(mat))
  rnames <- rep(rnames, ncol(mat))

  cnames <- if (use_names) colnames(mat)
  if (is.null(cnames)) cnames <- seq_len(ncol(mat))
  cnames <- rep(cnames, each = nrow(mat))

  result <- tibble::tibble(row = rnames, col = cnames, value = as.vector(mat))
  if (upper)
    return(result)
  result[as.vector(lower.tri(mat, diag = diag)), ]
}
