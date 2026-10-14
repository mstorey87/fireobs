#' Create a bounding box sf polygon
#'
#' @description
#' Takes an input sf polygon (e.g. fire boundary) and returns its bounding box as an sf polygon.
#' Optionally buffers the polygon before calculating the bounding box.
#' If `sf_polygon` is NULL, a default box around mainland Australia is returned.
#'
#' @param sf_polygon An sf object representing the fire boundary polygon (or area of interest).
#' @param bbox_buffer_km Distance in kilometres to buffer the polygon before creating the bounding box.
#' @param crs EPSG code for buffering step. Should be a projected CRS (e.g. 3112 for GDA94 / Geoscience Australia Lambert).
#'
#' @return An sf polygon of the bounding box, with the original CRS.
#' @export
#'
#' @examples
#' # Example: Create a bbox around a simple square polygon
#' library(sf)
#' p <- st_as_sf(st_sfc(st_polygon(list(rbind(
#'   c(150.0, -34.0), c(150.1, -34.0), c(150.1, -33.9), c(150.0, -33.9), c(150.0, -34.0)
#' ))), crs = 4326))
#' fire_bbox_polygon(p, bbox_buffer_km = 10, crs = 3112)
#'
#' # Example: Create default Australia bbox
#' fire_bbox_polygon(NULL, bbox_buffer_km = 0, crs = 3112)
fire_bbox_polygon <- function(sf_polygon = NULL,
                              bbox_buffer_km = 0,
                              crs = 3112) {

  # If no input polygon, use a default box around Australia in EPSG:4283
  if (is.null(sf_polygon)) {
    aus_wkt <- "SRID=4283;POLYGON ((111.3148 -45.36846, 155.6298 -45.36846, 155.6298 -8.916367, 111.3148 -8.916367, 111.3148 -45.36846))"
    sf_polygon <- sf::st_as_sf(data.frame(geometry = aus_wkt), wkt = "geometry")
  }

  # Dissolve multiple parts into one, ensure only one row
  sf_polygon <- sf_polygon %>%
    sf::st_make_valid() %>%      # repair self-intersections etc. before union
    sf::st_union() %>%
    sf::st_as_sf()

  # Explicitly set geometry column name
  sf::st_geometry(sf_polygon) <- "geometry"

  # Buffer, calculate bbox, convert to polygon, and back-transform to original CRS
  fire_bbox <- sf_polygon %>%
    sf::st_transform(crs) %>%                   # to projected CRS for buffering
    sf::st_buffer(bbox_buffer_km * 1000) %>%    # apply buffer in meters
    sf::st_transform(sf::st_crs(sf_polygon)) %>% # back to original CRS
    sf::st_bbox() %>%                           # create bbox
    sf::st_as_sfc() %>%                         # bbox to sfc polygon
    sf::st_as_sf()                              # sfc to sf

  # Carry over any non-geometry attributes if needed
  fire_bbox <- cbind(fire_bbox, sf::st_drop_geometry(sf_polygon))
  sf::st_geometry(fire_bbox) <- "geometry"

  return(fire_bbox)
}



