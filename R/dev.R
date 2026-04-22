### Binning/windows ------
# Smooth values of X by averaging across WINDOW_SIZE based on physical position POS
##' @export
smooth_physical <- function(x, pos, window_size = 1e5, na.rm = FALSE) {
  stopifnot(is.numeric(x))
  stopifnot(!is.na(x))
  stopifnot(is.numeric(pos))
  stopifnot(length(x) == length(pos))

  purrr::map_dbl(pos, function(point) {
    in_window <- which(pos > point - window_size & pos < point + window_size)
    mean(x[in_window], na.rm = na.rm)
  })
}

# Create sliding windows from START to END of SIZE. JUMP controls the distance
# between windows and defaults to SIZE. CONTIG is not used but added to the output
##' @export
sliding_window <- function(end,
                           start = 1,
                           size = 2e6,
                           jump = NULL,
                           contig = NULL) {
  if (is.null(jump))
    jump <- size
  # Compute number of windows
  remainder <- end - start - size + 1
  if (remainder < 1) {
    n <- 1
  } else {
    n <- ceiling(remainder / jump) + 1
  }
  # Generate windows
  windows <- vector("list", n)
  current <- start
  if (n > 1) {
    for (i in 1:(n - 1)) {
      last <- current + size - 1
      windows[[i]] <- list(start = current, end = last, contig = contig)
      current <- current + jump
    }
  }
  windows[[n]] <- list(start = current, end = end, contig = contig)
  windows
}
