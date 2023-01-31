##' @export
tabulate_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  stopifnot(is.list(cutoffs))

  result <- NULL
  for (current in names(cutoffs)) {
    min <- cutoffs[[current]]$min
    if (!is.null(min)) {
      result <- rbind(result, data.frame(
        filter = paste(current, ">=", min),
        n_fail = sum(df[[current]] < min, na.rm = TRUE)
      ))
    }

    max <- cutoffs[[current]]$max
    if (!is.null(max)) {
      result <- rbind(result, data.frame(
        filter = paste(current, "<=", max),
        n_fail = sum(df[[current]] > max, na.rm = TRUE)
      ))
    }
  }

  result <- result[order(result$n_fail, decreasing = TRUE), ]
  result$frac = round(result$n_fail / nrow(df), digits = 3)

  result
}
