##' @export
not_variable <- function(x, na_rm = TRUE) {
  stopifnot(is.vector(x))
  if (na_rm)
    x <- x[!is.na(x)]
  if (length(x) <= 1)
    return(TRUE)
  all(duplicated(x)[-1])
}
