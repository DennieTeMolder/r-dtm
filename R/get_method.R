# Ref: https://stackoverflow.com/questions/42738851/r-how-to-find-what-s3-method-will-be-called-on-an-object
# Example: GetMethod(print, vcf)
##' @export
get_method <- function(generic, ...) {
  ch <- deparse(substitute(generic))

  f <- X <- function(x, ...) UseMethod("X")
  for(m in utils::methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  fname <- X(...)
  class <- stringr::str_sub(fname, nchar(ch) + 2)

  cat(fname, "<- ")
  utils::getS3method(ch, class)
}
