test_that("fire_barra_sample_all works with multiple variables", {
  skip_on_cran()
  skip_if_offline()

  # Example input: simple sf POINT near Canberra
  my_points <- sf::st_sfc(
    sf::st_point(c(149.1, -35.3)),
    crs = 4326
  )
  my_sf <- sf::st_sf(id = 1, mytime = as.POSIXct("2019-12-01 12:00:00", tz = "UTC"), geometry = my_points)

  # Run function with two common variables
  result <- fire_barra_sample_all(
    dat = my_sf,
    time_col_utc = "mytime",
    barraid = "C2",
    varnames = c("tas", "sfcWind"),
    timestep = "daily",
    extract_fun = "mean"
  )

  # Should return sf with same number of rows
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(my_sf))

  # Should have added columns for tas_mean and sfcWind_mean
  expect_true(any(grepl("tas", names(result))))
  expect_true(any(grepl("sfcWind", names(result))))

  # Extracted values should be numeric or NA
  expect_true(all(sapply(result %>% st_drop_geometry %>% dplyr::select(contains("tas")), is.numeric)))
  expect_true(all(sapply(result%>% st_drop_geometry %>% dplyr::select(contains("sfcWind")), is.numeric)))

  # Geometry is valid
  expect_s3_class(sf::st_geometry(result), "sfc")
})
