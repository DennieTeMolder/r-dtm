##' @export
plotly_subplot <- function(..., title = NULL) {
  p <- plotly::subplot(
    ...,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.04
  )

  plotly::layout(p,
    title = title,
    margin = list(l = 65, r = 15, t = 60, b = 65, pad = 5)
  )
}

##' @export
plotly_json <- function(p, file) {
  stopifnot("plotly" %in% class(p))
  utils::capture.output(plotly::plotly_json(p, jsonedit = FALSE, pretty = FALSE), file = file)
}

##' @export
plotly_html <- function(p, file) {
  htmlwidgets::saveWidget(p, file, libdir = ".plotly")
}
