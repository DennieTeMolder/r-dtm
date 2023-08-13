##' @export
# Example: with_temp_lib(BiocManager::install("example_package"))
# OR with_temp_lib(remotes::install_cran("example_package"))
with_temp_lib <- function(expr, lib = tempdir()) {
  if (!file.exists(lib)) {
    cli::cli_alert_info(paste("Creating directory:", lib))
    dir.create(lib, recursive = TRUE)
  }
  add_lib_path(lib, append = FALSE)
  on.exit(add_lib_path(lib, append = TRUE))
  invisible(eval(expr))
}

##' @export
add_lib_path <- function(lib, append = TRUE) {
  stopifnot(file.exists(lib))
  old <- .libPaths()
  if (append) {
    new <- c(old[!old %in% lib], lib)
  } else {
    new <- unique(c(lib, old))
  }
  .libPaths(new)
}
