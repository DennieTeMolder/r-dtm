# return all rows in `x` with match in `y` in the order of `y`
##' @export
semi_join_reorder <- function(x, y, by, ...) {
  stopifnot(is.data.frame(x))
  stopifnot(is.data.frame(y) || is.vector(y))
  stopifnot(is.character(by))
  stopifnot(by %in% colnames(y))

  has_names <- !is.null(names(by))

  if (is.vector(y)) {
    if (length(by) != 1 || has_names)
      stop("When 'y' is a vector, 'by' should be a single value unnamed character vector!")
    y <- tibble::tibble(y)
    colnames(y) <- by
  } else {
    y <- y[, by, drop = FALSE]
  }

  # Rename columns of 'y' to be identical to 'x'
  if (has_names) {
    dict <- names(by)
    names(dict) <- by
    y <- dplyr::rename_with(y, .lookup_maybe, dict = dict)
    by <- names(by)
  }

  args <- list(x = y, y = x, by = by, ...)
  do.call(dplyr::left_join, args)
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
