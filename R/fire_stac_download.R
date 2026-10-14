#' Download Landsat and Sentinel-2 images and clip to fire extent.
#'
#' @description
#' Downloads Landsat and Sentinel-2 false-colour images using STAC API output.
#' Streams selected bands directly from cloud storage using GDAL `/vsicurl/`,
#' crops the imagery to the specified fire bounding box, merges the bands into
#' a false-colour composite, scales reflectance to 0-255, and writes the result
#' as a GeoTIFF to the specified output folder.
#'
#' Every image is scaled the same way (reflectance 0 to `reflectance_max` becomes 0 to 255,
#' brighter values are capped at 255), so colours can be compared between dates.
#'
#' This function is intended to be used with the output of `fire_stac_search()`.
#'
#' @param fire_bbox An `sf` polygon of the fire area of interest. Should match the input used for `fire_stac_search()`.
#' @param stac_df A data frame produced by `fire_stac_search()` containing the band URLs and metadata.
#' @param dest_folder Local folder path to save the cropped GeoTIFF images. Must already exist.
#' @param reflectance_max Surface reflectance value (DEA scale, 0-10000) mapped to 255. Default 3000.
#' @param max_tries Number of attempts for each image if there is a network error. Default 3.
#' @param wait_seconds Seconds to wait between attempts. Default 5.
#'
#' @return
#' Invisibly, a character vector of the GeoTIFF files written to `dest_folder`
#' (named `<local time>_<product>_<tile>_<UTC date>utc.tif`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' # Example bounding box: a square around an area of interest
#' fire_bbox <- st_as_sfc(st_bbox(c(xmin = 149.1, ymin = -35.4, xmax = 149.2, ymax = -35.3)), crs = 4326) %>%
#'   st_as_sf(crs=4326)
#'
#' # Define search time window
#' start_time <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2023-01-07", tz = "Australia/Sydney")
#'
#' # Run the search
#' stac_results <- fire_stac_search(fire_bbox, start_time, end_time)
#'
#' head(stac_results)
#'
#' outdir <- "C:/temp/landsat_sentinel"
#' dir.create(outdir)
#' fire_stac_download(fire_bbox, stac_results, outdir)
#' }
fire_stac_download <- function(fire_bbox, stac_df, dest_folder,
                               reflectance_max = 3000, max_tries = 3, wait_seconds = 5) {

  # Check inputs
  if (is.null(stac_df) || nrow(stac_df) == 0) {
    message("Nothing to download (stac_df is empty).")
    return(invisible(character(0)))
  }
  if (!dir.exists(dest_folder)) {
    stop("dest_folder does not exist: ", dest_folder, ". Create it first, e.g. dir.create(dest_folder).", call. = FALSE)
  }
  checkmate::assert_number(reflectance_max, lower = 1)

  # Stream, crop, combine and scale one image; returns the output file path, or NA if outside the bbox
  process_image <- function(dat) {
    band1 <- terra::rast(paste0("/vsicurl/", dat$band1))
    band2 <- terra::rast(paste0("/vsicurl/", dat$band2))
    band3 <- terra::rast(paste0("/vsicurl/", dat$band3))

    # Check overlap to avoid cropping outside extent
    b1_bbox <- sf::st_bbox(band1) %>% sf::st_as_sfc() %>% sf::st_as_sf()
    f_bbox <- sf::st_transform(fire_bbox, terra::crs(band1))
    if (lengths(sf::st_intersects(b1_bbox, f_bbox)) == 0) {
      message("Skipping ", dat$datetimelocal_chr, " ", dat$product, ": image outside fire_bbox")
      return(NA_character_)
    }

    # Crop bands and resample to the same grid
    b1 <- terra::crop(band1, f_bbox)
    b2 <- terra::resample(terra::crop(band2, f_bbox), b1)
    b3 <- terra::resample(terra::crop(band3, f_bbox), b1)

    # Same fixed scaling for every image so dates are comparable:
    # reflectance 0..reflectance_max -> 0..255, brighter values capped
    rgb <- c(b1, b2, b3)
    rgb <- terra::clamp(rgb, lower = 0, upper = reflectance_max, values = TRUE)
    rgb <- round(rgb * 255 / reflectance_max)

    out_file <- file.path(dest_folder, paste0(dat$datetimelocal_chr, "_", dat$product, "_",
                                              dat$tile_dateutc, "utc.tif"))
    # INT2U (not INT1U): INT1U uses 255 as its NA flag, which would turn the brightest pixels into NA
    terra::writeRaster(rgb, out_file, overwrite = TRUE, datatype = "INT2U")
    out_file
  }

  # Loop through images one by one, retrying network errors
  files <- character(0)
  for (i in seq_len(nrow(stac_df))) {
    dat <- stac_df[i, ]
    for (attempt in seq_len(max_tries)) {
      out <- tryCatch(process_image(dat), error = function(e) e)
      if (!inherits(out, "error")) break
      message("Attempt ", attempt, "/", max_tries, " failed for ", dat$datetimelocal_chr, " ",
              dat$product, ": ", conditionMessage(out))
      if (attempt < max_tries) Sys.sleep(wait_seconds)
    }
    if (inherits(out, "error")) {
      message("Skipping ", dat$datetimelocal_chr, " ", dat$product, " after ", max_tries, " attempts")
    } else if (!is.na(out)) {
      message("Saved ", basename(out))
      files <- c(files, out)
    }
  }

  message(length(files), " of ", nrow(stac_df), " images saved to ", dest_folder)
  invisible(files)
}
