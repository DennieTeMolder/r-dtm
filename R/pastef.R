# Version of paste0() that formats numeric values using format(big.mark)
##'@export
pastef <- function(..., sep = "", trim = TRUE) {
  args <- purrr::map_if(list(...), is.numeric, function(x) {
    format(x, big.mark = if (identical(getOption("OutDec"), ".")) "," else ".", trim = trim)
  })
  do.call(paste, c(args, sep = sep))
}
