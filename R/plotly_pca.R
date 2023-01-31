##' @export
plotly_pca <- function(pca,
                       comps = 1:2,
                       meta = NULL,
                       mode = "markers",
                       size = 9,
                       textposition = "top center",
                       legend = TRUE,
                       ...) {
  # Checks
  stopifnot(class(pca) == "prcomp")
  stopifnot(is.integer(comps))
  stopifnot(length(comps) == 2)
  if (!is.null(meta))
    stopifnot(is.data.frame(meta))

  # Convert the legend variable appropriately
  if (is.logical(legend)) {
    showlegend <- legend
    legend <- list(tracegroupgap = 0)
  } else if (is.character(legend)) {
    showlegend <- TRUE
    legend <- list(title = list(text = legend), tracegroupgap = 0)
  } else if (is.list(legend)) {
    showlegend <- TRUE
  } else {
    stop("The 'legend' argument can only be a logical, character, or list")
  }

  # % of variance explained per component
  explained <- pca_explained(pca)

  # Simplify
  df <- tibble::as_tibble(pca$x, rownames = "rownames")

  # Join
  if (!is.null(meta)) {
    if (is.null(meta[["rownames"]])) {
      stopifnot(nrow(df) == nrow(meta))
      df <- dplyr::bind_cols(df, meta)
    } else {
      stopifnot(df$rownames %in% meta$rownames)
      df <- dplyr::left_join(df, meta, by = "rownames")
    }
  }

  # If present use rownames as text labels
  if (is.null(df[["rownames"]])) {
    text <- NULL
  } else {
    text <- ~rownames
  }

  # Plot
  p <- plotly::plot_ly(
    df,
    x = stats::as.formula(paste0("~PC", comps[1])),
    y = stats::as.formula(paste0("~PC", comps[2])),
    type = "scatter",
    mode = mode,
    marker = list(
      size = size,
      line = list(color = "black", width = 1)
    ),
    text = text,
    textposition = textposition,
    showlegend = showlegend,
    ...
  )

  # Axis labels
  make_axis <- function(i)
    sprintf("PC%d (%.2f%%)", i, explained[i])

  plotly::layout(p,
    xaxis = list(title = make_axis(comps[1])),
    yaxis = list(title = make_axis(comps[2])),
    legend = legend
  )
}
