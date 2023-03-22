##' @export
map_cbind <- function(.x, .f, ..., .prefix = NULL) {
  df <- dplyr::bind_cols(purrr::map(.x, .f, ...))
  purrr::set_names(df, function(name) paste0(.prefix, name))
}
