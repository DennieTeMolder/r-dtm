### Exported ------
##' @export
# Function not exported from tibble
vectbl_restore <- getFromNamespace("vectbl_restore", "tibble")

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
num_unique <- function(..., na_rm = TRUE) {
  dplyr::n_distinct(..., na.rm = na_rm)
}

##' @export
pseq <- function(from, to, ...) {
  res <- mapply(seq.int, from = from, to = to, MoreArgs = list(...), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  do.call(c, res)
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
# Intrerlace multiple vectors
interlace <- function(...) {
  if (...length() > 1L)
    return(as.vector(rbind(...)))
  c(t(...))
}

##' @export
phred2prob <- function(x) {
  10^(x / -10)
}

##' @export
mem_size <- function(x) {
  pryr::object_size(x)
}

##' @export
substitute_dots <- function(...) {
  dots <- substitute(list(...))[-1L]
  sapply(dots, deparse)
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


### Internal ------
.flatten <- function(...) {
  stringr::str_flatten_comma(...)
}
