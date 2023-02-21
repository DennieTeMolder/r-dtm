##' @export
mem_size <- function(x) {
  pryr::object_size(x)
}

##' @export
replace_na <- function(x, replacement) {
  ifelse(is.na(x), replacement, x)
}

##' @export
apply_switch <- function(x, ...) {
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
num_unique <- function(x, na_rm = TRUE) {
  if (na_rm)
    x <- x[!is.na(x)]
  length(unique(x))
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
  if (!is.null(class) && !inherits(x, what = class))
    stop("Argument should be of class '", class, "'!")
  invisible(x)
}

##' @export
collapse <- function(x, delim = ", ") {
  paste(x, collapse = delim)
}

