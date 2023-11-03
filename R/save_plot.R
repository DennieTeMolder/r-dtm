# This is a wrapper of ggsave that will use defaults set by options(...)
# For compatibility with the Emacs ESS-plot package
##' @export
save_plot <- function(filename,
                      plot = ggplot2::last_plot(),
                      width = getOption("plot.width", NA),
                      height = getOption("plot.height", NA),
                      units = getOption("plot.units", "in"),
                      dpi = getOption("plot.res", 300),
                      ...) {
  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height,
                  units = units, dpi = dpi, ...)
}
