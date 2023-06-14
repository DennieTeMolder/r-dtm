##'@export
pastef <- function(..., sep = "") {
  args <- list(...)
  lapply(args, is.numeric)
  args <- purrr::map_if(args, is.numeric, function(x) {
    format(x, big.mark = ifelse(getOption("OutDec") == ".", ",", "."), trim = TRUE)
  })
  do.call(paste, c(args, sep = sep))
}
