##' @export
##' @importFrom rlang .data
plot_filters <- function(df, cutoffs, zoom = FALSE) {
  stopifnot(is.data.frame(df))
  .validate_cutoffs(cutoffs, available_cols = colnames(df))

  # Loop over all cutoffs and visualize
  plots <- lapply(names(cutoffs), function(current) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[current]]))

    # Check appropriate bin size
    n <- num_unique(df[[current]])
    if (n <= 100) {
      p <- p + ggplot2::geom_histogram(bins = n)
    } else {
      p <- p + ggplot2::geom_histogram(bins = 100)
    }

    # Add cutoffs
    min <- cutoffs[[current]]$min
    max <- cutoffs[[current]]$max
    if (!is.null(min))
      p <- p + ggplot2::geom_vline(xintercept = min, color = "blue")
    if (!is.null(max))
      p <- p + ggplot2::geom_vline(xintercept = max, color = "red")

    # Limit x-axis
    if (zoom) {
      x_lim <- rep(NA_real_, 2)
      ranges <- range(df[[current]], na.rm = TRUE)
      if (!is.null(min) && min < 0)
        x_lim[1] <- max(2 * min, ranges[1])
      if (!is.null(max))
        x_lim[2] <- min(2 * max, ranges[2])
      p <- p + xlim(x_lim)
    }

    p
  })

  names(plots) <- names(cutoffs)
  plots
}
