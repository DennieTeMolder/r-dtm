##' @export
rbind_list <- function(df_list, names_to = "names") {
  df <- do.call(dplyr::bind_rows, df_list)
  names <- lapply(names(df_list), function(key) {
    rep(key, nrow(df_list[[key]]))
  })
  names <- tibble::tibble(x = unlist(names, use.names = FALSE))
  colnames(names) <- names_to
  dplyr::bind_cols(names, df)
}
