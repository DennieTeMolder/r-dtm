##'@export
pastef <- function(..., sep = "") {
  args <- purrr::map_if(list(...), is.numeric, function(x) {
    format(x, big.mark = ifelse(getOption("OutDec") == ".", ",", "."), trim = TRUE)
  })
  do.call(paste, c(args, sep = sep))
}
