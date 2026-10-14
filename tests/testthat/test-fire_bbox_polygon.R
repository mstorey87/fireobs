test_that("fire_bbox_polygon works as expected", {

  # Load test dependencies
  library(sf)

  # Create a simple polygon: unit square in EPSG:4326
  p <- st_as_sf(st_sfc(st_polygon(list(rbind(
    c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
  ))), crs = 4326))

  # Run function with buffer
  bbox_poly <- fire_bbox_polygon(p, bbox_buffer_km = 10, crs = 3112)

  # Should return an sf polygon
  expect_s3_class(bbox_poly, "sf")

  # Should have geometry type POLYGON or MULTIPOLYGON
  expect_true(all(sf::st_geometry_type(bbox_poly) %in% c("POLYGON", "MULTIPOLYGON")))

  # Should have same CRS as input polygon
  expect_equal(sf::st_crs(bbox_poly), sf::st_crs(p))

  # The original polygon should be within the bbox
  expect_true(sf::st_within(p, bbox_poly, sparse = FALSE)[1])

  # Test default Australia box if NULL input
  aus_bbox <- fire_bbox_polygon(NULL, bbox_buffer_km = 0, crs = 3112)
  expect_s3_class(aus_bbox, "sf")
  expect_true(all(sf::st_geometry_type(aus_bbox) %in% c("POLYGON", "MULTIPOLYGON")))
})
