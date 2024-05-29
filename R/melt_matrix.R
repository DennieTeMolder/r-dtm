##' @export
melt_matrix <- function(mat, use_names = TRUE) {
  rnames <- if (use_names) rownames(mat)
  if (is.null(rnames)) rnames <- seq_len(nrow(mat))
  rnames <- rep(rnames, ncol(mat))

  cnames <- if (use_names) colnames(mat)
  if (is.null(cnames)) cnames <- seq_len(ncol(mat))
  cnames <- rep(cnames, each = nrow(mat))

  tibble(row = rnames, col = cnames, value = as.vector(mat))
}
