library(testthat)
library(sf)
library(terra)

test_that("fire_hotspot_map runs and produces output files", {
  skip_on_cran()
  skip_if_offline()

  # Define a small test polygon (Blue Mountains bbox approx)
  blue_mountains_box <- st_polygon(list(rbind(
    c(149.32276, -32.60165),  # top-left (xmin, ymax)
    c(151.61254, -32.60165),  # top-right (xmax, ymax)
    c(151.61254, -34.63015),  # bottom-right (xmax, ymin)
    c(149.32276, -34.63015),  # bottom-left (xmin, ymin)
    c(149.32276, -32.60165)   # close polygon
  ))) %>% st_sfc(crs = 4326)

  # Define short date range (2 days)
  start_time <- as.POSIXct("2019-11-21 00:00:00", tz = "Australia/Sydney")
  end_time <- as.POSIXct("2019-11-22 00:00:00", tz = "Australia/Sydney")

  # Use a temporary directory for outputs
  dest_folder <- tempdir()

  # Use a dummy or real mapkey here; replace with valid key if testing fully
  mapkey <- "dummy_mapkey_for_tests"

  # Run function with add_hotspots = FALSE (faster, no hotspot download)
  expect_error_free({
    fire_hotspot_map(
      fire_bbox = blue_mountains_box,
      start_time = start_time,
      end_time = end_time,
      mapkey = mapkey,
      dest_folder = dest_folder,
      add_hotspots = FALSE,
      outwidth = 200
    )
  })

  # Check that at least one TIFF file was created in the output folder
  tif_files <- list.files(dest_folder, pattern = "\\.tif$", full.names = TRUE)
  expect_true(length(tif_files) > 0)

  # Check that the TIFF files are readable rasters
  for (tif in tif_files) {
    r <- try(terra::rast(tif), silent = TRUE)
    expect_false(inherits(r, "try-error"))
    expect_true(terra::nlyr(r) >= 3)  # RGB has 3 layers
  }

  # Optionally, test with add_hotspots = TRUE but skip if mapkey is dummy
  if (mapkey != "dummy_mapkey_for_tests") {
    expect_error_free({
      fire_hotspot_map(
        fire_bbox = blue_mountains_box,
        start_time = start_time,
        end_time = end_time,
        mapkey = mapkey,
        dest_folder = dest_folder,
        add_hotspots = TRUE,
        outwidth = 200
      )
    })
  } else {
    message("Skipping add_hotspots=TRUE test with dummy mapkey")
  }
})
