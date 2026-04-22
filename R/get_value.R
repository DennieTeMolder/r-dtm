# Parse expression X and extract the values using the result from the object in
# left hand side of expression X
# Example: x <- 2:4; get_value(x %in% 1:3)
##' @export
get_value <- function(x) {
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
