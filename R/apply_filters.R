# Apply CUTOFFS to filter the rows of DF, see not_pass_filters()
##' @export
apply_filters <- function(df, cutoffs) {
  df[!not_pass_filters(df, cutoffs), ]
}
