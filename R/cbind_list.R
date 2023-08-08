##' @export
cbind_list <- function(x) {
  stopifnot(is.list(x))
  list_names <- names(x)
  x <- do.call(dplyr::bind_cols, c(x, .name_repair = "minimal"))
  if (!is.null(list_names))
    colnames(x) <- list_names
  x
}
