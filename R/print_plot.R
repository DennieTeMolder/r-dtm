##' @export
# Print and save plot to FILENAME at the same time unless `option(dtm.nosave = TRUE)`.
# If `option(dtm.print_plot != NULL)` the plot is written to `tempdir()` +
# /session_plots/ instead of printing.
print_plot <- function(p,
                       width = 7,
                       height = 5,
                       filename = NULL,
                       path = NULL,
                       ...) {
  inhibit_saving <- isTRUE(getOption("dtm.nosave"))
  print_ext <- getOption("dtm.print_plot")

  allowed_extentions <- c("png", "jpg", "jpeg", "pdf")
  if (isFALSE(print_ext %in% allowed_extentions))
    stop("Value of `options(dtm.print_plot='", print_ext , "')` not allowed! ",
         "Must be NULL or one of: ", .collapse(allowed_extentions))

  if (!is.null(print_ext)) {
    tmp_file <- tempfile(fileext = paste0(".", print_ext))
    ggplot2::ggsave(tmp_file, plot = p, width = width, height = height, ...)
    # Move the final file to the target folder to generate a single file-notify event
    print_dir <- file.path(tempdir(check = TRUE), "session_plots")
    n <- length(list.files(print_dir, pattern = "\\d{3}\\.\\w+"))
    print_file <- file.path(print_dir, sprintf("%03d.%s", n, print_ext))
    file.rename(tmp_file, print_file)
  } else {
    print(p)
  }

  if (is.null(filename))
    return(invisible(p))

  if (inhibit_saving) {
    message("Saving to file inhibited.")
  } else {
    if (isTRUE(tools::file_ext(filename) == print_ext)) {
      file.copy(print_file, filename, overwrite = TRUE)
    } else {
      ggplot2::ggsave(filename, plot = p, path = path,
                      width = width, height = height, ...)
    }
  }

  invisible(p)
}
