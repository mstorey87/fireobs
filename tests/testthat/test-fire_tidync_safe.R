test_that("fire_tidync_safe loads remote NetCDF or returns NULL on failure", {
  skip_on_cran()  # skip if running on CRAN to avoid network dependency
  skip_if_offline()  # skip if no internet

  # A known working NetCDF URL on NCI THREDDS (adjust if needed)
  nc_url <- "https://thredds.nci.org.au/thredds/dodsC/zv2/agcd/v2-0-3/precip/total/r001/01month/agcd_v2_precip_total_r001_monthly_2023.nc"

  result <- fire_tidync_safe(nc_url, max_tries = 2, wait_seconds = 2)
  expect_true(inherits(result, "tidync"))

  # Should return NULL if URL is fake
  bad_url <- "https://invalid.url/does_not_exist.nc"
  bad_result <- fire_tidync_safe(bad_url, max_tries = 2, wait_seconds = 1)
  expect_null(bad_result)
})
