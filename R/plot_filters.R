##' @export
##' @importFrom rlang .data
plot_filters <- function(df, cutoffs) {
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

    p
  })

  names(plots) <- names(cutoffs)
  plots
}
