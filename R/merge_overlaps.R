# Merge overlapping regions in DF into a single entry
##' @export
merge_annotations <- function(df) {
  stopifnot(is.data.frame(df), c("chrom", "start", "end") %in% colnames(df))

  if (nrow(df) < 1L)
    return(NULL)

  df |>
    count_annotations() |>
    dplyr::mutate(
      with_prev = .data$start == c(0L, .data$end[-dplyr::n()]),
      group = cumsum(!.data$with_prev),
      .by = "chrom"
    ) |>
    dplyr::summarise(
      chrom = .data$chrom[1L],
      start = .data$start[1L],
      end = .data$end[dplyr::n()],
      .by = c("chrom", "group")
    ) |>
    dplyr::select("chrom", "start", "end")
}
