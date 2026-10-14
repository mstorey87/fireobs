test_that("fire_stac_search returns valid image bands with https links", {
  skip_on_cran()
  skip_if_offline()

  library(sf)
  library(dplyr)

  # Small test bbox (Canberra region)
  fire_bbox <- st_as_sfc(
    st_bbox(c(xmin = 149.1, ymin = -35.4, xmax = 149.2, ymax = -35.3))
  ) %>% st_as_sf(crs = 4326)

  start_time <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
  end_time <- as.POSIXct("2023-01-07", tz = "Australia/Sydney")

  result <- fire_stac_search(fire_bbox, start_time, end_time)

  expect_s3_class(result, "data.frame")

  if (nrow(result) == 0) {
    skip("No images returned for this bbox and date range.")
  } else {
    expect_true(all(c("band1", "band2", "band3", "product", "datetimeutc") %in% names(result)))
    expect_gt(nrow(result), 0)
    expect_true(all(lubridate::year(result$datetimelocal) == 2023))

    # New check: band1, band2, band3 must all start with https
    expect_true(all(stringr::str_starts(result$band1, "https://")))
    expect_true(all(stringr::str_starts(result$band2, "https://")))
    expect_true(all(stringr::str_starts(result$band3, "https://")))
  }
})
