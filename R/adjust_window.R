##' @export
adjust_window <- function(window, span, adjust = "closest") {
  stopifnotsingle(window, class = "numeric")
  stopifnotsingle(span, class = "numeric")

  if (span <= window)
    return(span)

  # Adjust binwidth to fit more neatly over the span
  if (adjust == "closest")
    return(span / round(span / window))
  if (adjust == "upwards")
    return(span / floor(span / window))
  if (adjust == "downwards")
    return(span / ceiling(span / window))
  stop("The value adjust = '", adjust, "' is not recognised")
}
