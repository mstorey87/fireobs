test_that("fire_anu_sample returns sf with two expected columns and valid first value for fmc and flam", {
  skip_on_cran()
  skip_if_offline()

  my_line <- sf::st_sfc(
    sf::st_linestring(rbind(c(149.1, -35.3), c(149.2, -35.25))),
    crs = 4326
  )
  my_sf <- sf::st_sf(id = 1, geometry = my_line)

  vars_to_test <- c("fmc", "flam")

  for (v in vars_to_test) {
    result <- fire_anu_sample(
      datetimeutc = as.POSIXct("2019-01-01 00:00:00", tz = "UTC"),
      sf_data = my_sf,
      varname = v,
      allcells = FALSE
    )

    expect_s3_class(result, "sf")

    extracted_cols <- setdiff(names(result), names(my_sf))
    expect_true(length(extracted_cols) == 2, info = paste("Did not return 2 new columns for varname =", v))

    extracted_value <- result[[extracted_cols[1]]]
    expect_type(extracted_value, "double")
    expect_false(is.na(extracted_value), info = paste("First extracted value is NA for varname =", v))

    expect_s3_class(sf::st_geometry(result), "sfc")
  }
})
