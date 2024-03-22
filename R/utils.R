##' @export
mem_size <- function(x) {
  pryr::object_size(x)
}

##' @export
# To be used in combination with tryCatch(), e.g.:
# tryCatch(throw("my_condition"), my_condition = function(cnd) {})
# OR cnd <- catch("my_condition", expr = throw("my_condition"))
throw <- function(class = "dtm_throw", ..., message = NULL) {
  if (is.null(class))
    class <- "dtm_throw"
  rlang::abort(message = message, class = class, ...)
}

##' @export
# Wraps tryCatch() to return CND if it matches CLASSES instead of acting on it
# By default it won't catch regular R errors (unlike `catch_cnd`)
catch <- function(expr, classes = "dtm_throw") {
  rlang::catch_cnd(expr = expr, classes = classes)
}

##' @export
apply_switch <- function(x, ...) {
  lifecycle::deprecate_soft("5-3-2023", "dtm::apply_switch", "dplyr::case_match")
  sapply(x, function(y) switch(y, ..., NA))
}

##' @export
get_col <- function(n, mat) {
  stopifnot(is.matrix(mat))
  ceiling(n / nrow(mat))
}

##' @export
get_row <- function(n, mat) {
  stopifnot(is.matrix(mat))
  i <- n %% nrow(mat)
  i[i == 0] <- nrow(mat)
  i
}

##' @export
substitute_dots <- function(...) {
  dots <- substitute(list(...))[-1]
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
  10^(x/-10)
}

##' @export
get_sign <- function(x) {
  stopifnot(is.numeric(x))
  ifelse(x < 0, -1, 1)
}

##' @export
# Intrerlace multiple vectors
interlace <- function(...) {
  if (length(list(...)) > 1)
    return(c(rbind(...)))
  c(t(...))
}

##' @export
stopifnotsingle <- function(x, class = NULL) {
  if (length(x) != 1)
    stop("Argument should have a length of 1!")
  if (is.null(class))
    return(invisible(x))
  pass <- switch(class,
    null = TRUE,
    numeric = is.numeric(x),
    inherits(x, what = class)
  )
  if (!pass) {
    stop("Argument should be one of class '",
         stringr::str_flatten_comma(class), "'!")
  }
  invisible(x)
}

##' @export
pseq <- function(from, to, ...) {
  as.vector(mapply(seq.int, from = from, to = to, MoreArgs = list(...), USE.NAMES = FALSE))
}

##' @export
# Alternative to tidyr::replace_na() in which replacement can also be a vector
na_replace <- function(x, replacement) {
  stopifnot(is.vector(x))
  ifelse(is.na(x), replacement, x)
}

##' @export
get_col_rowwise <- function(mat, i) {
  stopifnot(is.matrix(mat))
  stopifnot(is.numeric(i), length(i) == nrow(mat))
  mat[cbind(seq_along(i), i)]
}

##' @export
mat2tibble <- function(mat) {
  if (tibble::is_tibble(mat))
    return(mat)
  stopifnot(is.matrix(mat) || is.vector(mat))

  if (is.vector(mat)) {
    mat <- matrix(mat, nrow = 1, dimnames = list(NULL, names(mat)))
  }

  mat <- as.data.frame(mat)
  tibble::as_tibble(mat, rownames = NA)
}

.flatten <- function(...) {
  stringr::str_flatten_comma(...)
}
