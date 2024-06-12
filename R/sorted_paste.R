##' @export
sorted_paste <- function(..., sep = " ") {
  dots <- lapply(list(...), as.character)
  names(dots) <- paste0("X", seq_along(dots))

  # Use a df to achieve proper recycling of arguments
  dots <- asplit(data.frame(dots), 1L)
  names(dots) <- NULL

  purrr::map_chr(dots, function(x) paste(sort(x), collapse = sep))
}
