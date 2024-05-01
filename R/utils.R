##' @export
mem_size <- function(x) {
  pryr::object_size(x)
}

##' @export
# To be used in combination with tryCatch() or catch(), e.g.:
# tryCatch(throw("my_condition"), my_condition = function(cnd) TRUE)
# OR cnd <- catch({print(1); throw(x = 3); print(2)})
# OR cnd <- catch("my_condition", expr = throw("my_condition"))
throw <- function(class = "dtm_throw", ...) {
  msg <- paste("condition with", ...length(), "included object(s)")
  rlang::signal(msg, class = class, ...)
}

##' @export
# Wraps tryCatch() to return CND if it matches CLASSES instead of acting on it
# By default it won't catch regular R errors (unlike `catch_cnd`)
catch <- function(expr, classes = "dtm_throw") {
  rlang::catch_cnd(expr = expr, classes = classes)
}

##' @export
apply_switch <- function(x, ...) {
  lifecycle::deprecate_soft("5-3-2023", "apply_switch()", "dplyr::case_match()")
  sapply(x, function(y) switch(y, ..., NA))
}

##' @export
get_col <- function(idx, mat) {
  stopifnot(is.matrix(mat))
  ceiling(idx / nrow(mat))
}

##' @export
get_row <- function(idx, mat) {
  stopifnot(is.matrix(mat))
  i <- idx %% nrow(mat)
  is_zero <- i == 0L
  if (any(is_zero))
    i[is_zero] <- nrow(mat)
  i
}

##' @export
substitute_dots <- function(...) {
  dots <- substitute(list(...))[-1L]
  sapply(dots, deparse)
}

##' @export
num_unique <- function(..., na_rm = TRUE) {
  dplyr::n_distinct(..., na.rm = na_rm)
}

# Version of `seq()` that always includes the number `to`
##' @export
seq_to_last <- function(from, to, ...) {
  x <- seq(from, to, ...)
  if (dplyr::last(x) != to)
    x <- c(x, to)
  x
}

##' @export
phred2prob <- function(x) {
  10^(x / -10)
}

##' @export
get_sign <- function(x) {
  lifecycle::deprecate_soft("1-5-2024", "get_sign()", "base::sign()")
  sign(x)
}

##' @export
# Intrerlace multiple vectors
interlace <- function(...) {
  if (...length() > 1L)
    return(c(rbind(...)))
  c(t(...))
}

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

##' @export
pseq <- function(from, to, ...) {
  c(mapply(seq.int, from = from, to = to, MoreArgs = list(...), USE.NAMES = FALSE))
}

##' @export
# Alternative to tidyr::replace_na() in which replacement can also be a vector
na_replace <- function(x, replacement) {
  stopifnot(length(replacement) == 1L || length(replacement) == length(x))
  is_na <- is.na(x)
  if (any(is_na)) {
    if (length(replacement) == 1L) {
      x[is_na] <- replacement
    } else {
      x[is_na] <- replacement[is_na]
    }
  }
  x
}

##' @export
get_col_rowwise <- function(mat, i) {
  lifecycle::deprecate_soft("1-5-2024", "get_col_rowwise()", "matrixStats::rowCollapse()")
  matrixStats::rowCollapse(x = mat, idxs = i)
}

##' @export
mat2tibble <- function(mat) {
  if (tibble::is_tibble(mat))
    return(mat)
  stopifnot(is.matrix(mat) || is.vector(mat))

  if (is.vector(mat)) {
    mat <- matrix(mat, nrow = 1L, dimnames = list(NULL, names(mat)))
  }

  mat <- as.data.frame(mat)
  tibble::as_tibble(mat, rownames = NA)
}

.flatten <- function(...) {
  stringr::str_flatten_comma(...)
}
