##' @export
apply_filters <- function(df, cutoffs) {
  df[!not_pass_filters(df, cutoffs), ]
}
