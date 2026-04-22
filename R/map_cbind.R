##' @export
map_cbind <- function(.x, .f, ..., .prefix = NULL) {
  lifecycle::deprecate_soft("22-4-2026", "map_cbind", "purrr::list_cbind()")

  df <- dplyr::bind_cols(purrr::map(.x, .f, ...))
  purrr::set_names(df, function(name) paste0(.prefix, name))
}
