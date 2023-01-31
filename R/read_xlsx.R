##' @export
read_xlsx <- function(path, na = c("", "NA"), ...) {
  readxl::read_excel(path, na = na, ...)
}
