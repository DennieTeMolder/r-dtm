##' @export
##' @importFrom rlang .data
plot_pca <- function(pca,
                     comps = 1:2,
                     size = 3,
                     names = NULL,
                     names_ratio = 1.25,
                     color_by = NULL,
                     fix_scales = FALSE,
                     ...) {
  stopifnot(class(pca) == "prcomp")
  stopifnot(is.integer(comps))
  stopifnot(length(comps) == 2)
  if (!is.null(names))
    stopifnot(nrow(pca$x) == length(names))
  if (!is.null(color_by))
    stopifnot(nrow(pca$x) == length(color_by))

  cols <- paste0("PC", comps)

  # % of variance explained per component
  explained <- pca_explained(pca)

  df <- as.data.frame(pca$x)

  if (!is.null(names))
    df <- cbind(df, names)

  if (!is.null(color_by))
    df <- cbind(df, color_by)

  p <- ggplot2::ggplot(df, ggplot2::aes(.data[[cols[1]]], .data[[cols[2]]], label = names, color = color_by)) +
    ggplot2::geom_point(size = size, ...) +
    ggplot2::xlab(sprintf("PC%d (%.2f%%)", comps[1], explained[comps[1]])) +
    ggplot2::ylab(sprintf("PC%d (%.2f%%)", comps[2], explained[comps[2]])) +
    ggsci::scale_color_lancet()

  if (fix_scales) {
    # Ensure equal scales
    limits <- range(unlist(df[, comps]))
    p <- p + ggplot2::xlim(limits) + ggplot2::ylim(limits)
  }

  if (!is.null(names))
    p <- p + ggrepel::geom_text_repel(point.padding = 0.1, size = size * names_ratio)

  p
}

# Percentage of explained variation
##' @export
pca_explained <- function(pca) {
  stopifnot(class(pca) == "prcomp")
  pca$sdev^2 / sum(pca$sdev^2) * 100
}
