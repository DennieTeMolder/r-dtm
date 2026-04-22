# Like purrr::list_cbind() but list elements are not required to be a dataframe
##' @export
cbind_list <- function(x) {
  lifecycle::deprecate_soft("22-4-2026", "cbind_list", "purrr::list_cbind()")

  stopifnot(is.list(x))
  list_names <- names(x)
  x <- do.call(dplyr::bind_cols, c(x, .name_repair = "minimal"))
  if (!is.null(list_names))
    colnames(x) <- list_names
  x
}
