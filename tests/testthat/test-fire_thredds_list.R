library(testthat)

test_that("fire_thredds_list returns filenames containing 'ABOM_OBS_'", {
  catalog_url <- "https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html"
  regex <- "ABOM_OBS_"

  result <- fire_thredds_list(catalog_url, regex)

  # Check that the result is a character vector and not NA
  expect_type(result, "character")
  expect_false(all(is.na(result)))

  # Check that all returned filenames contain the substring ABOM_OBS_
  expect_true(all(grepl("ABOM_OBS_", result)))

  # Optional: expect at least one file found
  expect_gt(length(result), 0)
})
