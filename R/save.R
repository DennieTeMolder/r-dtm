##' @export
toggle_save <- function() {
  new_val <- !save_p()
  options(dtm.save = new_val)
  message("Saving to file ", ifelse(new_val, "enabled", "disabled"))
  invisible(new_val)
}

##' @export
save_p <- function() {
  isTRUE(getOption("dtm.save", default = TRUE))
}
