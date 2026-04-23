# Read an .Rdata file and return the list of loaded objects
##' @export
read_rdata <- function(path) {
  objects <- mget(load(path))
  if (length(objects) == 1L)
    return(objects[[1L]])
  objects
}
