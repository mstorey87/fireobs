# tests/testthat/test-fire_RANN_nearest_points.R

test_that("fire_RANN_nearest_points() works correctly", {
  library(sf)

  # Create two simple overlapping squares
  poly1 <- st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  poly2 <- st_polygon(list(rbind(
    c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5)
  )))

  poly_sf1 <- st_sf(geometry = st_sfc(poly1),crs=4326)
  poly_sf2 <- st_sf(geometry = st_sfc(poly2),crs=4326)

  # Run function
  line_sf <- fire_RANN_nearest_points(
    poly = poly_sf2,
    poly.prior = poly_sf1,
    densify_m = 0,
    max_only = TRUE
  )

  # Test: Output is an sf object
  expect_s3_class(line_sf, "sf")

  # Test: Output geometry is LINESTRING
  expect_true(all(sf::st_geometry_type(line_sf) == "LINESTRING"))

  # Test: Line length is positive
  expect_true(all(as.numeric(sf::st_length(line_sf)) > 0))

  # Test: within_only option works
  # line_within <- fire_RANN_nearest_points(
  #   poly = poly_sf2,
  #   poly.prior = poly_sf1,
  #   densify_m = 0,
  #   within_only = TRUE,
  #   max_only = TRUE
  # )
  #
  # expect_s3_class(line_within, "sf")
  # expect_true(all(sf::st_geometry_type(line_within) == "LINESTRING"))
  # expect_true(all(as.numeric(sf::st_length(line_within)) > 0))
})
