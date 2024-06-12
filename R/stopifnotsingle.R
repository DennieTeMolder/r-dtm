##' @export
stopifnotsingle <- function(x, class = NULL, allow_null = FALSE, na_detect = FALSE) {
  # Check NULL
  if (allow_null && is.null(x))
    return(invisible(x))

  # Check class
  if (!is.null(class)) {
    pass <- switch(
      class,
      null = TRUE,
      numeric = is.numeric(x),
      inherits(x, what = class)
    )
    if (!pass)
      stop("`", substitute(x), "` should be one of class: ", .flatten(class), call. = FALSE)
  }

  # Check length
  if (length(x) != 1L)
    stop("`", substitute(x), "` should have a length of 1!", call. = FALSE)

  # Check NA/forbidden values
  if (!isFALSE(na_detect) && anyNA(x))
    stop("`", substitute(x), "` contains NA values!", call. = FALSE)
  if (!isTRUE(na_detect)) {
    forbidden <- na_detect %in% x
    if (any(forbidden)) {
      stop("`", substitute(x), "` contains forbidden values : ",
           .flatten(na_detect[forbidden]), call. = FALSE)
    }
  }

  invisible(x)
}
