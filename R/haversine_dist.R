# Ref: https://stackoverflow.com/a/28372037
#' @export
haversine_dist <- function(lon1, lat1, lon2, lat2) {
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  # Multiply by radius of the earth (in Km) to get distance
  c * 6378.137
}
