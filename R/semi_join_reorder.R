# return all rows in `y` with a match in `x` in the order of `x`.
# 'must_match' enforces all rows/values of 'x' to be matched.
##' @export
semi_join_reorder <- function(y,
                              x,
                              by,
                              must_match = TRUE,
                              allow_duplicate = FALSE,
                              ...) {
  stopifnot(is.data.frame(y))
  stopifnot(is.data.frame(x) || is.vector(x))
  stopifnot(is.character(by))

  has_names <- !is.null(names(by))

  if (is.vector(x)) {
    if (length(by) != 1 || has_names)
      stop("When 'x' is a vector, 'by' should be a single value unnamed character vector!")
    x <- tibble::tibble(x)
    colnames(x) <- by
  } else {
    stopifnot(by %in% colnames(x))
    x <- x[, by, drop = FALSE]
  }

  if (!allow_duplicate && anyDuplicated(x)) {
    stop("Duplicate values detected in 'x'!")
  }

  # Rename columns of 'x' to be identical to 'y'
  if (has_names) {
    rename_x <- names(by)
    rename_x[rename_x == ""] <- by[rename_x == ""]
    names(rename_x) <- by
    colnames(x) <- rename_x[colnames(x)]
    by <- unname(rename_x)
  }

  result <- dplyr::inner_join(x, y, by = by, relationship = "many-to-one", ...)

  if (must_match && nrow(result) != nrow(x))
    stop("Not all rows/values in 'x' match to 'y'!")

  result
}
