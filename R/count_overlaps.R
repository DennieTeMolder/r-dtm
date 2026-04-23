# Return a dataframe with the number of annotations per position
##' @export
count_annotations <- function(df) {
  stopifnot(is.data.frame(df), c("chrom", "start", "end") %in% colnames(df))

  df |>
    dplyr::select("chrom", "start", "end") |>
    tidyr::pivot_longer(c("start", "end"), values_to = "pos") |>
    dplyr::mutate(value = ifelse(.data$name == "start", 1L, -1L))|>
    dplyr::arrange(.data$chrom, .data$pos) |>
    dplyr::group_by(.data$chrom, .data$pos) |>
    dplyr::summarise(diff = sum(.data$value), .groups = "drop_last") |>
    dplyr::mutate(n = cumsum(.data$diff)) |>
    dplyr::mutate(end = c(.data$pos[-1L], NA)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$n > 0L) |>
    dplyr::select("chrom", start = "pos", "end", "n")
}
