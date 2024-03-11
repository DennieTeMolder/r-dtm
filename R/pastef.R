##'@export
pastef <- function(..., sep = "") {
  args <- purrr::map_if(list(...), is.numeric, function(x) {
    format(x, big.mark = if (identical(getOption("OutDec"), ".")) "," else ".", trim = TRUE)
  })
  do.call(paste, c(args, sep = sep))
}
