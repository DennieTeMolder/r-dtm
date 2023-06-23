##' @export
read_xlsx <- function(file, na = c("", "NA"), ...) {
  sheets <- readxl::excel_sheets(file)
  f <- function(sheet)
    readxl::read_excel(file, sheet = sheet, na = na, ...)
  if (length(sheets) < 2)
    return(f(sheets))
  sapply(sheets, f, simplify = FALSE, USE.NAMES = TRUE)
}
