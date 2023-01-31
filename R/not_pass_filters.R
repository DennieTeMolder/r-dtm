##' @export
not_pass_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  stopifnot(is.list(cutoffs))

  not_pass <- rep(FALSE, nrow(df))

  for (current in names(cutoffs)) {
    min <- cutoffs[[current]]$min
    if (!is.null(min))
      not_pass[df[[current]] < min] <- TRUE

    max <- cutoffs[[current]]$max
    if (!is.null(max))
      not_pass[df[[current]] > max] <- TRUE
  }

  not_pass
}
