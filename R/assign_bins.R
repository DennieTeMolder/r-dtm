##' @export
assign_bins <- function(x, bin_size, adjust = "closest") {
  stopifnot(is.vector(x))

  ranges <- range(x)
  span <- diff(ranges)
  bin_size <- adjust_window(bin_size, span, adjust)

  breaks <- seq_to_last(min(ranges) - 1, max(ranges), bin_size)
  bins <- cut(x, breaks, labels = FALSE)

  stopifnot(!is.na(bins))
  bins
}
