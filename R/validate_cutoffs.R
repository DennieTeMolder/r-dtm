.validate_cutoffs <- function(cutoffs, available_cols) {
  stopifnot(is.list(cutoffs))
  stopifnot(length(cutoffs) > 0)
  stopifnot(is.character(available_cols))

  all_valid <- TRUE

  for (current in names(cutoffs)) {
    if (!current %in% available_cols) {
      warning("Column not available: ", current, call. = FALSE)
      all_valid <- FALSE
    }

    if (!any(c("min", "max") %in% names(cutoffs[[current]]))) {
      warning("No min/max found for cutoff: ", current, call. = FALSE)
      all_valid <- FALSE
      next()
    }

    min <- cutoffs[[current]]$min
    max <- cutoffs[[current]]$max
    if (max(length(min), length(max)) != 1L) {
      warning("Min and/or max are not single length for cutoff: ", current, call. = FALSE)
      all_valid <- FALSE
    } else if (!is.null(min) && !is.numeric(min)) {
      warning("Min is not numeric for cutoff: ", current, call. = FALSE)
      all_valid <- FALSE
    } else if (!is.null(max) && !is.numeric(max)) {
      warning("Max is not numeric for cutoff: ", current, call. = FALSE)
      all_valid <- FALSE
    } else if (isTRUE(min >= max)) {
      warning("Min >= max for cutoff: ", current, call. = FALSE)
      all_valid <- FALSE
    }
  }

  if (!all_valid)
    stop("Not all provided cutoffs are valid! See `warnings()`.")

  invisible(all_valid)
}
