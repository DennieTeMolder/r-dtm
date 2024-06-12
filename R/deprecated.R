##' @export
apply_switch <- function(x, ...) {
  lifecycle::deprecate_soft("5-3-2023", "apply_switch()", "dplyr::case_match()")
  sapply(x, function(y) switch(y, ..., NA))
}

##' @export
get_sign <- function(x) {
  lifecycle::deprecate_soft("1-5-2024", "get_sign()", "base::sign()")
  sign(x)
}

##' @export
get_col_rowwise <- function(mat, i) {
  lifecycle::deprecate_soft("1-5-2024", "get_col_rowwise()", "matrixStats::rowCollapse()")
  matrixStats::rowCollapse(x = mat, idxs = i)
}
