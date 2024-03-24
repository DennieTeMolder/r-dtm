##' @export
tabulate_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  .validate_cutoffs(cutoffs, available_cols = colnames(df))

  mins <- lapply(names(cutoffs), function(current) {
    min <- cutoffs[[current]]$min
    if (is.null(min))
      return(NULL)
    data.frame(
      filter = paste(current, "<", min),
      n_removed = sum(df[[current]] < min, na.rm = TRUE)
    )
  })

  maxs <- lapply(names(cutoffs), function(current) {
    max <- cutoffs[[current]]$max
    if (is.null(max))
      return(NULL)
    data.frame(
      filter = paste(current, ">", max),
      n_removed = sum(df[[current]] > max, na.rm = TRUE)
    )
  })

  nas <- lapply(names(cutoffs), function(current) {
    na_rm <- cutoffs[[current]]$na.rm
    if (!isTRUE(na_rm))
      return(NULL)
    data.frame(
      filter = paste(current, "= NA"),
      n_removed = sum(is.na(df[[current]]))
    )
  })

  total <- list(data.frame(
    filter = "total",
    n_removed = sum(not_pass_filters(df, cutoffs = cutoffs))
  ))

  result <- do.call(rbind, c(mins, maxs, nas, total))
  result <- dplyr::arrange(result, .data$filter == "total", -.data$n_removed)
  result$frac = round(result$n_removed / nrow(df), digits = 3)

  result
}
