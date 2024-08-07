##' @export
read_xlsx <- function(file, sheet = NULL, na = c("", "NA"), ...) {
  if (is.null(sheet)) {
    sheets <- readxl::excel_sheets(file)
  } else {
    sheets <- sheet
  }

  f <- function(sheet) {
    readxl::read_excel(file, sheet = sheet, na = na, ...)
  }

  if (length(sheets) < 2)
    return(f(sheets))
  sapply(sheets, f, simplify = FALSE, USE.NAMES = TRUE)
}
