#' Create a ggplot map with an image and/or polygon overlay
#'
#' @description
#' Generates a quick map for visualising spatial data.
#' This function takes a `terra::rast()` raster image (such as an RGB image) and plots it using the `terrainr` package for RGB display.
#' To make plotting faster, the raster is aggregated with `terra::aggregate()` before plotting.
#' If a polygon (`sf` object) is also provided, it will be reprojected to match the image CRS (if needed) and overlaid on top.
#' If the polygon has a `firetype` column, it will colour polygons by `firetype` using a predefined palette.
#'
#' This is an internal helper for quick visual checks.
#'
#'
#' @param image A `terra::rast()` raster object. Should contain RGB bands named `red`, `green`, and `blue`.
#' @param prog An `sf` polygons object to overlay. Optional.
#' @param agg Integer aggregation factor passed to `terra::aggregate()` to downsample the raster for faster plotting.
#'
#' @return A `ggplot2` plot object.
#' @keywords internal
#'
#' @examples
#' # Example (not run):
#' # library(terra)
#' # library(sf)
#' # library(ggplot2)
#' #
#' # Example raster with RGB bands
#' # image <- rast(system.file("extdata/raster.tif", package = "terra"))
#' #
#' # Example polygon
#' # prog <- st_read(system.file("extdata/polygon.shp", package = "sf"))
#' #
#' # g <- fire_gg_map(image = image, prog = prog, agg = 5)
#' # print(g)
fire_gg_map <- function(image = NULL, prog = NULL, agg = 5) {

  g <- ggplot2::ggplot()

  if (!is.null(image)) {
    # Replace NA values with zero to ensure terrainr works
    image <- terra::subst(image, NA, 0)

    # Aggregate raster for faster plotting
    image <- terra::aggregate(image, agg)

    g <- g +
      terrainr::geom_spatial_rgb(
        data = image,
        mapping = ggplot2::aes(x = x, y = y, r = red, g = green, b = blue),
        na.rm = TRUE
      )
  }

  if (!is.null(prog)) {
    # Reproject polygon to match image CRS if image is provided
    if (!is.null(image)) {
      prog <- sf::st_transform(prog, sf::st_crs(image))
    }
  }

  if ("firetype" %in% names(prog)) {
    g <- g +
      ggplot2::geom_sf(data = prog, fill = NA, lwd = 0.8, ggplot2::aes(col = firetype)) +
      ggplot2::scale_color_manual(values = c("main" = "red", "spot" = "yellow", "backburn" = "pink"))
  } else {
    g <- g +
      ggplot2::geom_sf(data = prog, fill = NA, lwd = 0.8)
  }

  g <- g +
    ggplot2::coord_sf(lims_method = "geometry_bbox") +
    ggplot2::theme_minimal() +
    ggspatial::annotation_scale()

  return(g)
}
