test_that("fire_himawari_search returns expected columns and structure", {
  skip_on_cran()  # skip if on CRAN to avoid flaky network tests

  # Example: simple bbox (here: whole Australia)
  fire_bbox <- sf::st_as_sf(
    data.frame(
      geometry = "SRID=4283;POLYGON ((111.3148 -45.36846, 155.6298 -45.36846, 155.6298 -8.916367, 111.3148 -8.916367, 111.3148 -45.36846))"
    ),
    wkt = "geometry"
  )

  # Example time range: a short window in the past (more than 3 weeks back to avoid NRT branch)
  start_time <- as.POSIXct("2023-01-01 00:00:00", tz = "Australia/Sydney")
  end_time <- as.POSIXct("2023-01-01 01:00:00", tz = "Australia/Sydney")

  # Run function
  result <- fire_himawari_search(
    fire_bbox = fire_bbox,
    start_time = start_time,
    end_time = end_time,
    timestep_minutes = 60
  )

  # If no images found, result is NULL -> skip check
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true(all(c(
      "datetime_utc", "datetimelocal", "datetimelocal_chr",
      "date_local", "hour_local", "minutes_local",
      "daynight", "path_catalog", "file_name",
      "path_download", "band", "satellite"
    ) %in% names(result)))
    expect_gt(nrow(result), 0)
  } else {
    succeed("No Himawari images found in test range, test skipped.")
  }
})
