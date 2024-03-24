##' @export
##' @importFrom rlang .data
plot_filters <- function(df, cutoffs, bins = 100, zoom = TRUE) {
  stopifnot(is.data.frame(df), nrow(df) > 0)
  stopifnot(rlang::is_scalar_integerish(bins))
  stopifnot(rlang::is_bool(zoom))
  .validate_cutoffs(cutoffs, available_cols = colnames(df))

  # Loop over all cutoffs and visualize
  plots <- lapply(names(cutoffs), function(current) {
    curr_data <- df[[current]]
    curr_min <- cutoffs[[current]]$min
    curr_max <- cutoffs[[current]]$max

    # Remove NAs, track how many were removed
    if (anyNA(curr_data)) {
      na <- is.na(curr_data)
      curr_data <- curr_data[!na]
      na <- sum(na)
    } else {
      na <- 0L
    }

    if (is.logical(curr_data))
      curr_data <- as.integer(curr_data)

    # Limits of canvas
    x_lim <- range(curr_data, curr_min, curr_max)

    # Crop limits to zoom in on data close to CUTOFFS
    if (zoom) {
      # smaller == more zoomed in
      zoom_factor <- 4

      # Compute the size of a single zoom distance unit
      zoom_dist <- if (is.null(curr_min)) {
        abs(curr_max - median(curr_data))
      } else if (is.null(curr_max)) {
        abs(median(curr_data) - curr_min)
      } else {
        (curr_max - curr_min) / 2
      }

      # Re-calibrate the limits based on the zoom distance
      if (zoom_dist > .Machine$double.eps) {
        zoom_start <- if (is.null(curr_min)) {
          curr_max - zoom_dist * (zoom_factor - 1)
        } else {
          curr_min - zoom_dist
        }

        x_lim[1L] <- max(x_lim[1L], zoom_start)
        x_lim[2L] <- min(x_lim[2L], x_lim[1L] + zoom_dist * zoom_factor)
      }
    }

    # Compute breaks, using a rounded bin width.
    binwidth <- signif(diff(x_lim) / bins, digits = 1L)
    breaks <- seq(x_lim[1L], x_lim[2L] + binwidth, by = binwidth)

    # Covert back to df
    curr_df <- data.frame(x = curr_data)
    colnames(curr_df) <- current

    # Base plot
    na_action <- if ("keep_na" %in% names(cutoffs[[current]])) "kept" else "removed"
    subtitle <- if (zoom) sprintf(
      "Percentage of total data omitted: %.1f%% left; %.1f%% right",
      ceiling(mean(curr_data < min(breaks)) * 1000) / 10,
      ceiling(mean(curr_data >= max(breaks)) * 1000) / 10
    )
    p <- ggplot2::ggplot(curr_df, ggplot2::aes(x = .data[[current]])) +
      ggplot2::ggtitle(paste(na, "NAs", na_action), subtitle = subtitle) +
      ggplot2::geom_histogram(breaks = breaks, closed = "left")

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
