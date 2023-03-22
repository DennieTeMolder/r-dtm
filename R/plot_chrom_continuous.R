# This function only provides the canvas, you still need to add a geom_.
# This function creates/modifies df$pos_adj and aes$x
##' @export
##' @importFrom rlang .data
plot_chrom_continuous <- function(df, aes = NULL, sizes = NULL) {
  # Check input
  stopifnot(is.data.frame(df), c("chrom", "pos") %in% colnames(df))
  stopifnot(is.character(df$chrom), !is.na(df$chrom))
  stopifnot(is.numeric(df$pos), !is.na(df$pos))
  if (!is.null(sizes)) {
    stopifnot(is.data.frame(sizes), ncol(sizes) == 2)
    stopifnot(is.character(sizes[[1]]), !duplicated(sizes[[1]]))
    stopifnot(is.numeric(sizes[[2]]))

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
    sizes <- dplyr::filter(sizes, .data$chrom %in% used_chroms)
  }

  # Compute position adjustment
  size_order <- order(sizes$len, decreasing = TRUE)
  adjust_pos <- cumsum(sizes$len[size_order])
  adjust_pos <- c(0, utils::head(adjust_pos, n = -1))
  names(adjust_pos) <- sizes$chrom[size_order]
  df$pos_adj <- df$pos + adjust_pos[df$chrom]

  # Modify aesthetic mappings
  aes <- .modify_aes(aes, x = .data$pos_adj)
  if (is.null(aes$text)) {
    .make_label <- function(chrom, pos)
      paste0(format(pos, big.mark = ",", trim = TRUE), "bp ", chrom)
    aes <- .modify_aes(aes, text = .make_label(.data$chrom, .data$pos))
  }

  # Compute x limits
  factor <- 0.04
  total_size <- sum(sizes$len)
  xlim <- c(0 + factor * total_size, total_size - factor * total_size)

  # Plot
  ggplot2::ggplot(df, aes) +
    ggplot2::scale_x_continuous(breaks = adjust_pos, minor_breaks = NULL) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 315, hjust = 0))
}
