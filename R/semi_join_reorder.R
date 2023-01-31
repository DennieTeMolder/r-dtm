# return all rows in `x` with match in `y` in the order of `y`
##' @export
semi_join_reorder <- function(x, y, by, ...) {
  if (is.vector(y) && length(by) == 1 && is.null(names(by))) {
    y <- tibble::tibble(y1 = y)
    colnames(y) <- by
  } else {
    y <- y[, by]
  }
  if (!is.null(names(by))) {
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
