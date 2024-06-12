##' @export
assert_length <- function(x, len, what = NULL, na_detect = FALSE, allow_null = FALSE) {
  len <- as.integer(len)

  # Check NULL
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(x))
    } else {
      stop("`", substitute(x), "` cannot be NULL!", call. = FALSE)
    }
  }

  # Check class
  if (is.null(what)) {
    # Do nothing
  } else if (is.function(what)) {
    if (!what(x))
      stop("`", substitute(what), "(", substitute(x), ")` is not TRUE!", call. = FALSE)
  } else if (!inherits(x, what = what)) {
    stop("`", substitute(x), "` should be one of class: ", .flatten(class), "!", call. = FALSE)
  }

  # Check length
  if (length(x) != len)
    stop("`", substitute(x), "` should have a length of ", len, "!", call. = FALSE)

  # Check NA/forbidden values
  if (identical(na_detect, FALSE)) {
    # Do nothing
  } else if (identical(na_detect, TRUE)) {
    if (anyNA(x))
      stop("`", substitute(x), "` contains NA values!", call. = FALSE)
  } else if (!is.null(na_detect)) {
    forbidden <- na_detect %in% x
    if (any(forbidden)) {
      stop("`", substitute(x), "` contains the following forbidden value(s): ",
           .flatten(na_detect[forbidden]), "!", call. = FALSE)
    }
  }

  invisible(x)
}
