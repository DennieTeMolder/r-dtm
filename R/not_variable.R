##' @export
not_variable <- function(x, na_rm = TRUE) {
  lifecycle::deprecate_soft("22-4-2026", "not_variable", "varkit::has_no_variation()")

  stopifnot(is.vector(x))
  if (na_rm)
    x <- x[!is.na(x)]
  if (length(x) <= 1)
    return(TRUE)
  all(duplicated(x)[-1])
}
