library(testthat)
library(sf)
library(geosphere)

test_that("fire_get_line_info calculates correct bearing and supports reverse", {
  # Create a line approx 10 km northeast from start point
  start <- c(150.0, -33.0)
  end <- geosphere::destPoint(start, 45, 10000)  # 10 km at 45 degrees
  line <- st_linestring(rbind(start, end))
  sf_line <- st_sf(geometry = st_sfc(line, crs = 4326))

  # Normal (no reverse)
  res <- fire_get_line_info(sf_line, reverse = FALSE)

  expect_equal(round(res$bearing_epsg4326, 0), 45)
  expect_equal(round(res$line_km_epsg4326, 1), 10)

  # Reversed start and end
  res_rev <- fire_get_line_info(sf_line, reverse = TRUE)

  # Bearing should be 180 degrees opposite (45 + 180 = 225)
  expect_equal(round(res_rev$bearing_epsg4326, 0), 225)

  # Distance should remain the same
  expect_equal(round(res_rev$line_km_epsg4326, 1), 10)

  # Start and end coordinates should be swapped in reversed output
  expect_equal(res$start_x_epsg4326, res_rev$end_x_epsg4326)
  expect_equal(res$start_y_epsg4326, res_rev$end_y_epsg4326)
  expect_equal(res$end_x_epsg4326, res_rev$start_x_epsg4326)
  expect_equal(res$end_y_epsg4326, res_rev$start_y_epsg4326)
})
