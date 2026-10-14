# tests/testthat/test-fire_search_hotspots.R

test_that("fire_search_hotspots returns valid sf object with expected columns", {
  # Skip if no internet or mapkey is missing
  skip_if_not(Sys.getenv("FIRMS_MAPKEY") != "", "No FIRMS API key found in environment.")

  # Dummy bounding box: small area around Canberra
  fire_bbox <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = 148.8, ymin = -35.5, xmax = 149.4, ymax = -35.1), crs = 4326)
  )

  # Use your mapkey from environment
  mapkey <- "a5452249ca7c7a4ee2e1e6da787f57cc"

  # Temporary folder
  dest_folder <- tempdir()


  # Run the function for a very short time window to keep result small
  result <- fire_search_hotspots(
    fire_bbox = fire_bbox,
    mapkey = mapkey,
    start_date = "2024-01-01",
    end_date = "2024-02-31",
    dest_folder = dest_folder,
    product_filter = "VIIRS"
  )

  # Should return an sf object or empty tibble if no fires found
  expect_true(inherits(result, "sf") | nrow(result) == 0)

  # If not empty, check that expected columns exist
  if (nrow(result) > 0) {
    expect_true(all(c("datetimeutc", "datetimelocal", "sat_name") %in% colnames(result)))
    expect_s3_class(result$geometry, "sfc_POINT")
  }
})
