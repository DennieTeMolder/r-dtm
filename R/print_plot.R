##' @export
# Print and save plot to filename at the same time unless `option(inhibit_save = TRUE)`.
# If `option(print2pdf = TRUE)` a pdf is written to `tempdir()` instead of printing.
print_plot <- function(p,
                       width = 7,
                       height = 5,
                       filename = NULL,
                       path = NULL,
                       ...) {
  print2pdf <- getOption("print2pdf", default = FALSE)

  if (print2pdf) {
    dir <- paste0(tempdir(check = TRUE), "/emacs_plots")
    n <- length(list.files(dir, pattern = "\\d{3}.pdf"))
    emacs_filename <- sprintf("%03d.pdf", n)
    emacs_file <- ggplot2::ggsave(emacs_filename, plot = p, path = dir,
                                  width = width, height = height, ...)
  } else {
    print(p)
  }

  if (is.null(filename))
    return(invisible(p))

  if (getOption("inhibit_save", default = FALSE)) {
    message("Saving to file inhibited.")
  } else {
    if (print2pdf && stringr::str_ends(filename, "\\.pdf")) {
      file.copy(emacs_file, filename, overwrite = TRUE)
    } else {
      ggplot2::ggsave(filename, plot = p, path = path,
                      width = width, height = height, ...)
    }
  }

  invisible(p)
}
