##' @export
# Print and save plot to filename at the same time unless `option(inhibit_save = TRUE)`.
# If `option(print2pdf = TRUE)` a pdf is written to `tempdir()` instead of printing.
print_plot <- function(p,
                       width = 5,
                       height = 4,
                       filename = NULL,
                       path = NULL,
                       ...) {
  print2pdf <- getOption("print2pdf", default = FALSE)
  do_save <- !(is.null(filename) || getOption("inhibit_save", default = FALSE))

  if (print2pdf) {
    dir <- tempdir(check = TRUE)
    n <- length(list.files(dir, pattern = "emacs_plot_\\d{3}.pdf"))
    emacs_filename <- sprintf("emacs_plot_%03d.pdf", n)
    emacs_file <- ggplot2::ggsave(emacs_filename, plot = p, path = dir,
                                  width = width, height = height, ...)
  } else {
    print(p)
  }

  if (do_save) {
    if (print2pdf && stringr::str_ends(filename, "\\.pdf")) {
      file.copy(emacs_file, filename, overwrite = TRUE)
    } else {
      ggplot2::ggsave(filename, plot = p, path = path,
                      width = width, height = height, ...)
    }
  }

  invisible(p)
}
