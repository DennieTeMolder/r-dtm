# Returns a boolean vector that is TRUE for each row of dataframe DF that
# doesn't mean any of the criteria in CUTOFFS. CUTOFFS should be a list with the
# format CUTOFFS[df_column_name][min|max|na.rm].
##' @export
not_pass_filters <- function(df, cutoffs) {
  stopifnot(is.data.frame(df))
  .validate_cutoffs(cutoffs, available_cols = colnames(df))

  not_pass <- rep(FALSE, nrow(df))

  for (current in names(cutoffs)) {
    min <- cutoffs[[current]]$min
    max <- cutoffs[[current]]$max
    na_rm <- cutoffs[[current]]$na.rm
    if (!is.null(min))
      not_pass[df[[current]] < min] <- TRUE
    if (!is.null(max))
      not_pass[df[[current]] > max] <- TRUE
    if (isTRUE(na_rm) && anyNA(df[[current]]))
      not_pass[is.na(df[[current]])] <- TRUE
  }

  not_pass
}
