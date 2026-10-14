test_that("fire_get_timezone returns expected timezone for NSW bbox", {
  library(sf)

  # Example bbox roughly covering NSW (longitude 140-150, latitude -37 to -28)
  bbox <- st_as_sfc(st_bbox(c(xmin = 140, xmax = 150, ymin = -37, ymax = -28), crs = 4326))
  fire_bbox <- st_sf(geometry = bbox)

  tz <- fire_get_timezone(fire_bbox)

  # Should return a character string matching one of the Australian time zones
  expect_type(tz, "character")
  expect_true(tz %in% fireobs::dat.timezone.names$tz_name)
})
