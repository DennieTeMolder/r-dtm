# Assign bin numbers to the values of X based on BIN_SIZE adjusted by
# adjust_window(adjust = ADJUST)
##' @export
assign_bins <- function(x, bin_size, adjust = "closest") {
  stopifnot(is.vector(x))

  ranges <- range(x)
  span <- diff(ranges)

  if (span == 0)
    return(rep(1L, length(x)))

  bin_size <- adjust_window(bin_size, span, adjust)
  breaks <- seq_to_last(min(ranges) - 1L, max(ranges), bin_size)
  bins <- cut(x, breaks, labels = FALSE)

  stopifnot(!is.na(bins))
  bins
}
