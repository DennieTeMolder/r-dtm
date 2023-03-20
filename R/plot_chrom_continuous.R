# This function only provides the canvas, you still need to add a geom_
##' @export
##' @importFrom rlang .data
plot_chrom_continuous <- function(data, aes = NULL, sizes = NULL) {
  # Check input
  stopifnot(is.data.frame(data))
  stopifnot(c("chrom", "pos") %in% colnames(data))
  stopifnot(!is.na(data$chrom), !is.na(data$pos))
  if (!is.null(sizes)) {
    stopifnot(is.data.frame(sizes))
    stopifnot(c("chrom", "len") %in% colnames(sizes))
    used_chroms <- unique(data$chrom)
    if (!all(used_chroms %in% sizes$chrom))
      stop("Not all values of 'chrom' are present in `", substitute(sizes), "`!")
  }

  # Create missing input
  if (is.null(sizes)) {
    # Scaffolds are often larger, but this is a good approximation
    sizes <- dplyr::summarise(dplyr::group_by(data, .data$chrom), len = max(.data$pos))
  } else {
    sizes <- dplyr::filter(sizes, .data$chrom %in% used_chroms)
  }

  # Compute position adjustment
  size_order <- order(sizes$len, decreasing = TRUE)
  adjust_pos <- cumsum(sizes$len[size_order])
  adjust_pos <- c(0, utils::head(adjust_pos, n = -1))
  names(adjust_pos) <- sizes$chrom[size_order]

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
  data$pos_adj <- data$pos + adjust_pos[data$chrom]

  # Plot
  ggplot2::ggplot(data, aes) +
    ggplot2::scale_x_continuous(breaks = adjust_pos, minor_breaks = NULL) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 315, hjust = 0))
}
