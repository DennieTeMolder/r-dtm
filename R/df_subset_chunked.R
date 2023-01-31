# For use with functions like `readr::read_tsv_chunked()`
##' @export
df_subset_chunked <- function(df, i, irow = NULL, icol = NULL) {
  if (is.null(irow))
    irow <- seq_len(nrow(df))
  if (is.null(icol))
    icol <- seq_len(ncol(df))

  irow <- irow - (i - 1)
  irow <- irow[irow > 0 & irow <= nrow(df)]
  df[irow, icol]
}
