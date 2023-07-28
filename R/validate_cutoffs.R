.validate_cutoffs <- function(cutoffs, available_cols) {
  stopifnot(is.list(cutoffs))
  stopifnot(length(cutoffs) > 0)
  stopifnot(is.character(available_cols))

  all_valid <- TRUE

  for (current in names(cutoffs)) {
    if (!current %in% available_cols) {
      warning("Column '", current, "' not found in data frame!")
      all_valid <- FALSE
    }
    if (!any(c("min", "max") %in% names(cutoffs[[current]]))) {
      warning("No min/max found for cutoff: ", current)
      all_valid <- FALSE
    }

    min <- cutoffs[[current]]$min
    max <- cutoffs[[current]]$max
    if (isTRUE(min >= max)) {
      warning("Min >= max for cutoff: ", current)
      all_valid <- FALSE
    }
  }

  if (!all_valid)
    stop("Not all provided cutoffs are valid! See `warnings()`.")

  invisible(all_valid)
}
