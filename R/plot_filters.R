##' @export
##' @importFrom rlang .data
plot_filters <- function(df, cutoffs, bins = 100, min_binwidth = 0.01) {
  stopifnot(is.data.frame(df))
  stopifnot(rlang::is_scalar_integerish(bins))
  stopifnot(rlang::is_scalar_double(min_binwidth))
  .validate_cutoffs(cutoffs, available_cols = colnames(df))

  # Loop over all cutoffs and visualize
  plots <- lapply(names(cutoffs), function(current) {
    curr_col <- df[[current]]
    curr_min <- cutoffs[[current]]$min
    curr_max <- cutoffs[[current]]$max

    # Remove NAs, track how many were removed
    if (anyNA(curr_col)) {
      na <- is.na(curr_col)
      curr_col <- curr_col[!na]
      na <- sum(na)
    } else {
      na <- 0L
    }

    if (is.logical(curr_col))
      curr_col <- as.integer(curr_col)

    # Limits of canvas
    x_lim <- range(0L, curr_col, curr_min, curr_max)

    # TODO zoom based on quantile distribution

    # Compute breaks, make last bin right inclusive
    binwidth <- max(min_binwidth, diff(x_lim) / bins)
    breaks <- seq_to_last(x_lim[1L], x_lim[2L], by = binwidth)
    breaks[length(breaks)] <- breaks[length(breaks)] + binwidth

    # Covert back  to df
    curr_col <- data.frame(x = curr_col)
    colnames(curr_col) <- current

    # Base canvas
    na_action <- if ("keep_na" %in% names(cutoffs[[current]])) "kept" else "removed"
    p <- ggplot2::ggplot(curr_col, ggplot2::aes(x = .data[[current]])) +
      ggplot2::ggtitle(paste(na, "NAs", na_action)) +
      ggplot2::geom_histogram(breaks = breaks)

    # Add cutoffs
    if (!is.null(curr_min))
      p <- p + ggplot2::geom_vline(xintercept = curr_min, color = "blue")
    if (!is.null(curr_max))
      p <- p + ggplot2::geom_vline(xintercept = curr_max, color = "red")

    p
  })

  names(plots) <- names(cutoffs)
  plots
}
