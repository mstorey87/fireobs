test_that("fire_max_spread_line returns expected structure and values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("checkmate")
  skip_if_not_installed("stringr")
  skip_if_not_installed("fireobs") # assumes you have the helper function

  library(sf)
  library(lubridate)

  # CRS: GDA94 / MGA zone 56 (projected meters)
  crs_proj <- 3112

  # Center point roughly near Canberra
  center <- c(149.1, -35.8)  # lon, lat

  # Project center to projected CRS coords
  center_proj <- sf::st_transform(
    sf::st_sfc(sf::st_point(center), crs = 4326),
    crs_proj
  )
  center_proj_xy <- sf::st_coordinates(center_proj)

  # Simple helper to create ellipse
  create_simple_ellipse <- function(center_xy, a, b, n = 20, angle = 0) {
    t <- seq(0, 2*pi, length.out = n)
    x <- a * cos(t)
    y <- b * sin(t)
    theta <- angle * pi / 180
    rot_mat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2)
    coords <- cbind(x, y) %*% rot_mat
    coords[,1] <- coords[,1] + center_xy[1]
    coords[,2] <- coords[,2] + center_xy[2]
    coords <- rbind(coords, coords[1,])  # close
    st_polygon(list(coords))
  }

  # Two ellipses: small inside large
  poly2 <- create_simple_ellipse(center_proj_xy, a = 2000, b = 1000, angle = 30)
  poly1 <- create_simple_ellipse(center_proj_xy - 900, a = 500, b = 300, angle = 30)

  fire_polygons <- sf::st_sf(
    fire_id = c("fire_1", "fire_1"),
    date = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 14:00:00"), tz = "Australia/Sydney"),
    geometry = sf::st_sfc(poly1, poly2, crs = crs_proj)
  )

  spread_lines <- fire_max_spread_line(
    polygons = fire_polygons,
    time_col = "date",
    id_col = "fire_id",
    convex_hull = TRUE,
    max_only = TRUE,
    internal_only = FALSE,
    min_minutes = 30,
    max_minutes = 360,
    densify_m = 50
  )

  # Basic checks
  expect_s3_class(spread_lines, "sf")
  expect_true(nrow(spread_lines) >= 1)
  expect_true(all(c("start_time", "end_time", "line_km", "ros_kmh", "min_time_any") %in% names(spread_lines)))
  expect_gt(unique(spread_lines$line_km), 0)
  expect_gt(unique(spread_lines$ros_kmh), 0)
  expect_false(any(is.na(spread_lines$min_time_any)))
})
