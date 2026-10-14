test_that("fire_stac_download creates cropped GeoTIFFs", {
  skip_on_cran()
  skip_if_offline()

  library(sf)
  library(dplyr)
  library(terra)

  # Test bbox: small area with known coverage
  fire_bbox <- st_as_sfc(
    st_bbox(c(xmin = 149.1, ymin = -35.4, xmax = 149.2, ymax = -35.3))
  ) %>% st_as_sf(crs = 4326)

  start_time <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
  end_time <- as.POSIXct("2023-01-07", tz = "Australia/Sydney")

  # Search for images
  stac_df <- fire_stac_search(fire_bbox, start_time, end_time)

  if (nrow(stac_df) == 0) {
    skip("No images found — skipping download test.")
  }

  # Use a temporary directory for output
  outdir <- tempdir()

  # Run download
  fire_stac_download(fire_bbox, stac_df[1, ], outdir)

  # Find GeoTIFFs in the temp directory
  tifs <- list.files(outdir, pattern = "\\.tif$", full.names = TRUE)

  expect_gt(length(tifs), 0)
  expect_true(all(file.exists(tifs)))

  # Check that each file is readable as a raster
  for (f in tifs) {
    r <- terra::rast(f)
    expect_s4_class(r, "SpatRaster")
    expect_true(all(dim(r)[1:2] > 0))
  }
})
