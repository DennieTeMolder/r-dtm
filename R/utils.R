### Exported ------
##' @export
# Function not exported from tibble
vectbl_restore <- getFromNamespace("vectbl_restore", "tibble")

# Get the col number of index IDX in MAT
##' @export
get_col <- function(idx, mat) {
  stopifnot(is.matrix(mat))
  ceiling(idx / nrow(mat))
}

# Get the row number of index IDX in MAT
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
  lifecycle::deprecate_soft("22-4-2026", "num_unique", "dplyr::n_distinct()")
  dplyr::n_distinct(..., na.rm = na_rm)
}

# Version of seq() where FROM and TO can each be a vector
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
# Interlace multiple vectors
interlace <- function(...) {
  if (...length() > 1L)
    return(as.vector(rbind(...)))
  c(t(...))
}

# Convert Phred-scores to probability values
##' @export
phred2prob <- function(x) {
  10^(x / -10)
}

# Deparse the ... expression
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
