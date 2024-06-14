## Split 'varkit_index' objects into sub-contigs
##' @export
split_index <- function(index, bin_size = 1e5, adjust = "closest") {
  stopifnot(is.data.frame(index))
  stopifnot(c("id", "linum", "n_lines") %in% colnames(index))

  res <- Map(function(id, linum, n_lines) {
    if (n_lines > bin_size) {
      end <- linum + n_lines - 1
      bin_size <- adjust_window(bin_size, span = n_lines, adjust = adjust)
      linum <- floor(seq(linum, end, by = bin_size))
      n_lines <- diff(c(linum, end))
    }
    data.frame(id, linum, n_lines)
  }, index$id, index$linum, index$n_lines)
  res <- do.call(dplyr::bind_rows, res)

  # Restore attributes
  vectbl_restore(res, index)
}
