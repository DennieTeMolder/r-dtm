##' @export
##' @importFrom rlang .data
plot_pca <- function(pca,
                     comps = 1:2,
                     names = NULL,
                     colorBy = NULL,
                     fixScales = FALSE,
                     ...) {
  stopifnot(class(pca) == "prcomp")
  stopifnot(is.integer(comps))
  stopifnot(length(comps) == 2)
  if (!is.null(names))
    stopifnot(nrow(pca$x) == length(names))
  if (!is.null(colorBy))
    stopifnot(nrow(pca$x) == length(colorBy))

  cols <- paste0("PC", comps)

  # % of variance explained per component
  explained <- pca_explained(pca)

  df <- as.data.frame(pca$x)

  if (!is.null(names))
    df <- cbind(df, names)

  if (!is.null(colorBy))
    df <- cbind(df, colorBy)

  p <- ggplot2::ggplot(df, ggplot2::aes(.data[[cols[1]]], .data[[cols[2]]], label = names, color = colorBy)) +
    ggplot2::geom_point(...) +
    ggplot2::xlab(sprintf("PC%d (%.2f%%)", comps[1], explained[comps[1]])) +
    ggplot2::ylab(sprintf("PC%d (%.2f%%)", comps[2], explained[comps[2]])) +
    ggsci::scale_color_lancet()

  if (fixScales) {
    # Ensure equal scales
    limits <- unlist(pca$x[,comps])
    limits <- c(min(limits), max(limits))
    p <- p + ggplot2::xlim(limits) + ggplot2::ylim(limits)
  }

  if (!is.null(names))
    p <- p + ggrepel::geom_text_repel(point.padding = 0.1)

  return(p)
}

# Percentage of explained variation
##' @export
pca_explained <- function(pca) {
  stopifnot(class(pca) == "prcomp")
  pca$sdev^2 / sum(pca$sdev^2) * 100
}
