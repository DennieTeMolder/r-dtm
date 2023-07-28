##' @export
tabulate_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  .validate_cutoffs(cutoffs, available_cols = colnames(df))

  result <- NULL
  for (current in names(cutoffs)) {
    min <- cutoffs[[current]]$min
    if (!is.null(min)) {
      result <- rbind(result, data.frame(
        filter = paste(current, "<", min),
        n_removed = sum(df[[current]] < min, na.rm = TRUE)
      ))
    }

    max <- cutoffs[[current]]$max
    if (!is.null(max)) {
      result <- rbind(result, data.frame(
        filter = paste(current, ">", max),
        n_removed = sum(df[[current]] > max, na.rm = TRUE)
      ))
    }
  }

  result <- rbind(
    result[order(result$n_removed, decreasing = TRUE), ],
    data.frame(filter = "total", n_removed = sum(not_pass_filters(df, cutoffs)))
  )
  result$frac = round(result$n_removed / nrow(df), digits = 3)

  result
}
