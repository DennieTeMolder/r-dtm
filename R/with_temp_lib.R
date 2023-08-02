##' @export
# Example: with_temp_lib(BiocManager::install("example_package"))
# OR with_temp_lib(remotes::install_cran("example_package"))
with_temp_lib <- function(expr) {
  .libPaths(unique(c(tempdir(), .libPaths())))
  on.exit(.libPaths(c(.libPaths()[-1], tempdir())))
  invisible(eval(expr))
}
