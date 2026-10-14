library(testthat)
library(sf)

test_that("fire_himawari_download downloads files for search results", {
  skip_on_cran()
  skip_if_offline()

  # Define a small bbox polygon in EPSG:4326 (replace with small area)
  dat.bbox <- st_as_sfc("POLYGON((150 -34, 150 -33.95, 150.05 -33.95, 150.05 -34, 150 -34))", crs = 4326)

  # Define short start and end times (use recent date to avoid no data)
  datestart <- as.POSIXct(Sys.Date() - 10, tz = "Australia/Sydney")
  dateend <- datestart + 3600 * 2  # 2 hour window

  # Run search function with 60 min timestep
  search_results <- fire_himawari_search(
    fire_bbox = dat.bbox,
    start_time = datestart,
    end_time = dateend,
    timestep_minutes = 60
  )

  # Expect that search_results is a data frame with certain columns
  expect_s3_class(search_results, "data.frame")
  expect_true(all(c("path_download", "band", "datetimelocal_chr") %in% names(search_results)))

  # Create temporary directory for downloads
  tmp_dir <- tempdir()

  # Run download function; expect no error
  expect_error(
    fire_himawari_download(
      fire_bbox = dat.bbox,
      df_download = search_results,
      dest_folder = tmp_dir
    ),
    NA
  )

  # After download, check if tif files exist in temp folder
  tif_files <- list.files(tmp_dir, pattern = "\\.tif$", full.names = TRUE)
  expect_true(length(tif_files) > 0)
})
