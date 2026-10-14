test_that("fire_stac_sample_veg works for daily, yearly fractional cover and SRTM DEM products", {
  library(sf)
  library(dplyr)

  # Example line near Canberra (should intersect valid tiles)
  my_sf <- sf::st_sfc(
    sf::st_linestring(
      matrix(
        c(149.5, -35.4,
          149.6, -35.4),
        ncol = 2, byrow = TRUE
      )
    )
  ) %>%
    sf::st_as_sf(crs = 4326)

  # Define time window for daily product (needs >14 days)
  start_time_daily <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
  end_time_daily <- as.POSIXct("2023-01-20", tz = "Australia/Sydney")

  # Run the query for daily fractional cover
  result_daily <- fire_stac_sample_veg(
    sf_object = my_sf,
    start_time = start_time_daily,
    end_time = end_time_daily,
    collection_name = "ga_ls_fc_3"
  )

  # Expect output is a data frame or NULL
  expect_true(is.null(result_daily) || inherits(result_daily, "data.frame"))
  if (!is.null(result_daily)) {
    expect_true(any(grepl("pv", names(result_daily))))
    expect_true(all(c("cell_x", "cell_y") %in% names(result_daily)))
  }

  # Define time window for yearly product (just start_time needed)
  start_time_yearly <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
  end_time_yearly <- NULL  # yearly product only needs start_time

  start_time_yearly_utc=lubridate::with_tz(start_time_yearly,"UTC")

  # Run the query for yearly fractional cover
  result_yearly <- fire_stac_sample_veg(
    sf_object = my_sf,
    start_time = start_time_yearly,
    end_time = end_time_yearly,
    collection_name = "ga_ls_fc_pc_cyear_3"
  )

  # Expect output is a data frame or NULL
  expect_true(is.null(result_yearly) || inherits(result_yearly, "data.frame"))
  if (!is.null(result_yearly)) {
    expect_true(any(grepl("cyear", names(result_yearly))))
    expect_true(all(c("cell_x", "cell_y") %in% names(result_yearly)))
    expect_true(all(lubridate::year(result_yearly$datetime)==lubridate::year(start_time_yearly_utc)))
  }

  # For SRTM DEM product, no time needed (both start and end NULL)
  result_dem <- fire_stac_sample_veg(
    sf_object = my_sf,
    start_time = NULL,
    end_time = NULL,
    collection_name = "ga_srtm_dem1sv1_0"
  )

  # Expect output is a data frame or NULL
  expect_true(is.null(result_dem) || inherits(result_dem, "data.frame"))
  if (!is.null(result_dem)) {
    # DEM product may not have sample_name pattern same as others
    expect_true(all(c("cell_x", "cell_y") %in% names(result_dem)))
  }
})
