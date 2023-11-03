# This function only provides the canvas, you still need to add a geom_.
# This function creates/modifies df$.pos_adj and aes$x
##' @export
##' @importFrom rlang .data
plot_chrom_continuous <- function(df, aes = NULL, sizes = NULL) {
  # Check input
  stopifnot(is.data.frame(df), c("chrom", "pos") %in% colnames(df))
  stopifnot(is.character(df$chrom), !is.na(df$chrom))
  stopifnot(is.numeric(df$pos), !is.na(df$pos))
  if (!is.null(sizes)) {
    stopifnot(is.data.frame(sizes), dplyr::between(ncol(sizes), 2, 3))
    stopifnot(is.character(sizes[[1]]), !anyDuplicated(sizes[[1]]))
    stopifnot(is.numeric(sizes[[2]]))
    if (ncol(sizes) == 3)
      stopifnot(is.character(sizes[[3]]))

    used_chroms <- unique(df$chrom)
    if (!all(used_chroms %in% sizes[[1]]))
      stop("Not all values of 'chrom' are present in sizes[[1]]!")
  }

  # Create missing input
  if (is.null(sizes)) {
    # Scaffolds are often larger, but this is a good approximation
    sizes <- dplyr::summarise(dplyr::group_by(df, .data$chrom), len = max(.data$pos))
  } else {
    colnames(sizes)[1:2] <- c("chrom", "len")
    sizes <- dplyr::filter(dplyr::ungroup(sizes), .data$chrom %in% used_chroms)
  }

  # Compute position adjustment
  sizes <- dplyr::arrange(sizes, -.data$len)
  adjust_pos <- cumsum(utils::head(sizes$len, n = -1))
  adjust_pos <- c(0, adjust_pos)
  names(adjust_pos) <- sizes$chrom
  df$.pos_adj <- df$pos + adjust_pos[df$chrom]

  breaks <- c(adjust_pos, sum(sizes$len))
  if (ncol(sizes) == 3)
    names(breaks) <- c(sizes[[3]], "")

  # Modify aesthetic mappings
  if (is.null(aes)) {
    aes <- ggplot2::aes(x = .data$.pos_adj)
  } else {
    aes$x <- ggplot2::aes(x = .data$.pos_adj)$x
  }

  # Plot
  ggplot2::ggplot(df, aes) +
    ggplot2::scale_x_continuous(breaks = breaks, minor_breaks = NULL) +
    coord_trim_x(max(breaks)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 315, hjust = 0))
}
