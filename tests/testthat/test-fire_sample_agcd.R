test_that("fire_sample_agcd() works with real AGCD 2023 NetCDF", {
  library(sf)
  library(lubridate)

  skip_on_cran() # skip on CRAN to avoid external server dependency

  # 1. Make a test point in Australia for January 2023
  pts <- st_sf(
    geometry = st_sfc(st_point(c(150, -35))),
    datetime_utc = as.POSIXct("2023-01-15 00:00:00", tz = "UTC"),
    crs = 4326
  )

  # 2. Run the function with a small buffer
  result <- fire_sample_agcd(pts, time_utc_col = "datetime_utc", buffer_m = 20000)

  # 3. Structure checks
  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "agcd_precip_mean",
    "agcd_precip_min",
    "agcd_precip_max",
    "agcd_precip_median",
    "agcd_date"
  ) %in% names(result)))

  # 4. Check that precip values are numeric and reasonable
  expect_type(result$agcd_precip_mean, "double")
  expect_true(all(result$agcd_precip_mean >= 0))
  expect_true(all(result$agcd_precip_max >= result$agcd_precip_min))

  # 5. Check that returned date is correct month
  expect_equal(lubridate::year(result$agcd_date), 2023)
  expect_equal(lubridate::month(result$agcd_date), 1)
})
