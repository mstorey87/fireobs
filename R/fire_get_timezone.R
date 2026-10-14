#' Get Australian time zone of a fire bounding box centroid
#'
#' @description
#' Calculates the centroid of the input fire bounding box polygon,
#' finds the nearest Australian state polygon (from `fireobs::dat.aus`),
#' then returns the corresponding time zone name (e.g. `"Australia/Sydney"`).
#'
#' @param fire_bbox An `sf` polygon object representing the fire bounding box.
#'
#' @return A string with the Australian time zone name.
#' @export
#'
#' @examples
#' # Example using a bounding box covering NSW
#' library(sf)
#' bbox <- st_as_sfc(st_bbox(c(xmin = 140, xmax = 150, ymin = -37, ymax = -28), crs = 4326))
#' fire_bbox <- st_sf(geometry = bbox)
#' fire_get_timezone(fire_bbox)
fire_get_timezone <- function(fire_bbox) {

  dat.aus <- fireobs::dat.aus  %>%
    dplyr::mutate(name=ifelse(name=="Jervis Bay Territory","New South Wales",name))# Australian states polygons

  # Get the centroid of the fire bounding box, transform to match states CRS
  dat.cent <- fire_bbox %>%
    sf::st_union() %>%
    sf::st_centroid() %>%
    sf::st_transform(3112) %>%
    sf::st_as_sf()

  # Find the nearest state polygon and get its timezone
  dat.tz <- dat.aus[sf::st_nearest_feature(dat.cent, dat.aus), ] %>%
    dplyr::left_join(fireobs::dat.timezone.names, by = "name")

  return(dat.tz$tz_name)
}
