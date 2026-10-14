test_that("fire_barra_path constructs valid URLs", {
  # Test inputs
  datetime <- as.POSIXct("2019-12-01 10:00:00", tz = "UTC")

  # Test for each barraid and timestep
  for (barraid in c("R2", "C2")) {
    for (timestep in c("hourly", "daily", "monthly")) {
      url <- fire_barra_path(datetime, barraid, timestep, "sfcWind")

      # Check the return type
      expect_type(url, "character")
      expect_true(nzchar(url)) # non-empty string

      # Check url contains barraid code and timestep folder
      expect_true(grepl(barraid, url))
      expected_folder <- switch(timestep,
                                hourly = "1hr",
                                daily = "day",
                                monthly = "mon")
      expect_true(grepl(expected_folder, url))

      # Check that url ends with .nc
      expect_true(grepl("\\.nc$", url))
    }
  }

  # Expect error for invalid barraid
  expect_error(fire_barra_path(datetime, "invalid", "hourly", "sfcWind"))

  # Expect error for invalid timestep
  expect_error(fire_barra_path(datetime, "R2", "invalid", "sfcWind"))

  # Expect error for invalid varname type
  expect_error(fire_barra_path(datetime, "R2", "hourly", 123))
})
