##' @export
rbind_list <- function(df_list, names_to = "names", ...) {
  lifecycle::deprecate_soft("5-3-2023", "rbind_list", "purrr::list_rbind()")
  purrr::list_rbind(df_list, names_to = names_to, ...)
}
