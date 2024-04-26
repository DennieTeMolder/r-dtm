#' @export
haversine_dist <- function(lon, lat, name = NULL) {
  stopifnot(is.double(lon), is.double(lat), length(lon) == length(lat))
  if (!is.null(name))
    stopifnot(length(name) == length(lon))

  len <- length(lon)
  dnames <- list(name, name)
  if (len == 1L)
    return(matrix(0, dimnames = dnames))

  # Compute the comparisons for the lower triangle
  comps <- seq_len(len)
  comps <- expand.grid(i = comps, j = comps, KEEP.OUT.ATTRS = FALSE)
  comps <- comps[comps$i > comps$j, ]

  res <- matrix(0, nrow = len, ncol = len, dimnames = dnames)
  res[as.matrix(comps)] <- with(comps, .haversine_dist_internal(lon[i], lat[i], lon[j], lat[j]))
  res[as.matrix(comps[2:1])] <- res[as.matrix(comps)]
  res
}

# Ref: https://stackoverflow.com/a/28372037
.haversine_dist_internal <- function(lon1, lat1, lon2, lat2) {
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  # Multiply by radius of the earth (in Km) to get distance
  c * 6378.137
}
