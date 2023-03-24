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
    dict <- names(by)
    names(dict) <- by
    x <- dplyr::rename_with(x, .lookup_maybe, dict = dict)
    by <- names(by)
  }

  result <- dplyr::inner_join(x, y, by = by, relationship = "many-to-one", ...)

  if (must_match && nrow(result) != nrow(x))
    stop("Not all rows/values in 'x' match to 'y'!")

  result
}

.lookup_maybe <- function(x, dict) {
  if(!is.vector(dict) || is.list(dict))
    stop("`dict` should be a named vector!")
  if (is.null(names(dict)))
    stop("`dict` does not have any names!")
  new <- dict[x]
  new <- ifelse(is.na(new), x, new)
  names(new) <- NULL
  new
}
