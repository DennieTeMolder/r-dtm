##' @export
# Example: with_temp_lib(BiocManager::install("example_package"))
# OR with_temp_lib(remotes::install_cran("example_package"))
with_temp_lib <- function(expr, lib = tempdir()) {
  .libPaths(unique(c(lib, .libPaths())))
  on.exit(.libPaths(c(.libPaths()[.libPaths() != lib], lib)))
  invisible(eval(expr))
}
