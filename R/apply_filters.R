##' @export
##' @importFrom rlang .data
apply_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  stopifnot(is.list(cutoffs))

  for (current in names(cutoffs)) {
    min <- cutoffs[[current]]$min
    if (!is.null(min))
      df <- dplyr::filter(df, .data[[current]] >= min)

    max <- cutoffs[[current]]$max
    if (!is.null(max))
      df <- dplyr::filter(df, .data[[current]] <= max)
  }

  df
}
