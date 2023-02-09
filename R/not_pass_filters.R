##' @export
not_pass_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  stopifnot(is.list(cutoffs))

  not_pass <- rep(FALSE, nrow(df))

  for (current in names(cutoffs)) {
    if (!any(c("min", "max") %in% names(cutoffs[[current]]))) {
      warning("No min/max found for: ", current)
      next()
    }
    if (!current %in% colnames(df))
      stop("Column '", current, "' not found in ", substitute(df))

    min <- cutoffs[[current]]$min
    max <- cutoffs[[current]]$max

    if (isTRUE(min >= max))
      stop("Min >= max for: ", current)

    if (!is.null(min))
      not_pass[df[[current]] < min] <- TRUE
    if (!is.null(max))
      not_pass[df[[current]] > max] <- TRUE
  }

  not_pass
}
