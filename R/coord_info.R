# Retrieve country level information of lat lon vectors
##' @export
coord_info <- function(lon, lat, scale = c("small", "medium", "large"), digits = 5) {
  rlang::check_installed("rnaturalearth")
  rlang::check_installed("sf")
  scale <- match.arg(scale)

  # Round values and find unique coordinates
  data <- tibble::tibble(lon = round(lon, digits = digits), lat = round(lat, digits = digits))
  uniq <- dplyr::distinct(stats::na.omit(data))

  # Create coordinate objects
  worldmap <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")
  sf <- sf::st_as_sf(uniq, coords = c("lon", "lat"), remove = FALSE)
  sf <- sf::st_set_crs(sf, sf::st_crs(worldmap))

  # Intersect coordinates with the worldmap
  geoinfo <- suppressMessages({
    sf::sf_use_s2(FALSE)
    suppressWarnings(sf::st_intersection(sf, worldmap))
  })

  # Restore the original order
  dplyr::left_join(data, geoinfo, by = c("lon", "lat"))
}
