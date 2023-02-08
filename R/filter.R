# Example: filter(x %in% 1:3)
##' @export
filter <- function(x) {
  stopifnot(is.logical(x) || is.numeric(x))

  call <- substitute(x)
  lhs <- .get_lhs(call)

  eval(lhs)[x]
}

.get_lhs <- function(call) {
  stopifnot(is.call(call))
  if (deparse(call[[1]]) == "!")
    return(.get_lhs(call[[2]]))
  call[[2]]
}
