#' Download Himawari images from BOM THREDDS server
#'
#' @description
#' Accepts the output of \code{\link{fire_himawari_search}()} to clip and download data from BOM's NCI THREDDS server.
#' Because Himawari data is coarse resolution, the \code{fire_bbox} polygon is buffered by 100 km before clipping.
#' Different band combinations can be selected; the default (bands 7, 4, 1) is good for visualizing active fires.
#' Output GeoTIFF files are multi-band composites (e.g. RGB) for daytime images when all \code{bands} are available,
#' otherwise single-band (first band in \code{bands}), e.g. for nighttime images.
#'
#' Each Himawari file is a 10-minute scan of the full disk, from north to south, and its file name time is the
#' start of that scan. The time the fire area was actually observed is several minutes later
#' (about +7 min over Darwin, +9 min over Sydney and Hobart). This observation time is estimated from the
#' file's \code{scan_line_time} variable over the clipped area, and is returned and included in the output file name.
#'
#' @param fire_bbox An \code{sf} polygon defining the fire bounding box.
#' @param df_download Data frame of Himawari paths returned from \code{\link{fire_himawari_search}()}.
#' @param bands Character vector of Himawari bands to download. Default is \code{c("B07","B04","B01")}.
#'   The first band is used when a multi-band image is not possible (night, or a band is missing). Can be a single band.
#' @param dest_folder Local folder path to save downloaded GeoTIFF files. Created if it does not exist.
#' @param stretch Logical. If \code{TRUE} (default), each band is stretched to 0-255 (2nd-98th percentile) for viewing.
#'   If \code{FALSE}, original values are kept (e.g. B07 brightness temperature in K) for analysis.
#' @param max_tries Number of attempts for each time-step before skipping it. Default 2.
#' @param wait_seconds Seconds to wait between attempts. Default 10.
#'
#' @return Invisibly, a data frame with one row per time-step: \code{datetime_utc} (file / scan start time),
#'   \code{obs_time_utc} and \code{obs_time_local} (estimated time the fire area was observed),
#'   \code{bands} (bands written), \code{file} (output path, \code{NA} if failed) and \code{status}
#'   (\code{"ok"} or the error message). GeoTIFF files are written to \code{dest_folder}, named
#'   \code{<file time local>_<satellite>_<bands>_obs<observed time local>.tif}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Define a bounding box polygon (replace with your actual bbox)
#' dat.bbox <- st_as_sfc("POLYGON((150 -34, 150 -33.5, 150.5 -33.5, 150.5 -34, 150 -34))", crs = 4326)
#'
#' # Define start and end times for search (POSIXct)
#' datestart <- as.POSIXct("2023-01-01 00:00:00", tz = "Australia/Sydney")
#' dateend <- as.POSIXct("2023-01-02 00:00:00", tz = "Australia/Sydney")
#'
#' # Search for Himawari images (e.g., every 60 minutes)
#' dat.himawari <- fire_himawari_search(
#'   fire_bbox = dat.bbox,
#'   start_time = datestart,
#'   end_time = dateend,
#'   timestep_minutes = 60
#' )
#'
#' # Download the first two time-steps
#' first_two <- head(unique(dat.himawari$datetime_utc), 2)
#' res <- fire_himawari_download(
#'   fire_bbox = dat.bbox,
#'   df_download = dat.himawari[dat.himawari$datetime_utc %in% first_two, ],
#'   dest_folder = file.path(tempdir(), "himawari")
#' )
#' res
#' }
fire_himawari_download <- function(fire_bbox,
                                   df_download,
                                   bands = c("B07", "B04", "B01"),
                                   dest_folder,
                                   stretch = TRUE,
                                   max_tries = 2,
                                   wait_seconds = 10) {

  # Check inputs
  if (is.null(df_download) || nrow(df_download) == 0) {
    message("Nothing to download (df_download is empty)")
    return(invisible(data.frame()))
  }
  checkmate::assert_data_frame(df_download)
  needed_cols <- c("datetime_utc", "datetimelocal", "datetimelocal_chr", "daynight",
                   "band", "path_download", "satellite")
  missing_cols <- setdiff(needed_cols, names(df_download))
  if (length(missing_cols) > 0) {
    stop("df_download is missing columns: ", paste(missing_cols, collapse = ", "),
         ". Use the output of fire_himawari_search().")
  }
  checkmate::assert_character(bands, min.len = 1, any.missing = FALSE, unique = TRUE)
  missing_bands <- setdiff(bands, df_download$band)
  if (length(missing_bands) > 0) {
    stop("Band(s) not found in df_download: ", paste(missing_bands, collapse = ", "),
         ". Available: ", paste(sort(unique(df_download$band)), collapse = ", "))
  }
  checkmate::assert_string(dest_folder)
  checkmate::assert_flag(stretch)
  checkmate::assert_count(max_tries, positive = TRUE)
  checkmate::assert_number(wait_seconds, lower = 0)

  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)

  # Himawari geostationary projection
  geos_crs <- "+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs"
  local_tz <- lubridate::tz(df_download$datetimelocal)

  # Transform bbox to Himawari CRS, buffering first to capture a wider area
  bbox_him <- fire_bbox %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(100000) %>%
    sf::st_transform(geos_crs) %>%
    sf::st_bbox()

  # Keep requested bands, in the order given in `bands`
  df_download <- df_download %>%
    dplyr::filter(band %in% bands) %>%
    dplyr::arrange(datetime_utc, match(band, bands))

  times_unique <- unique(df_download$datetime_utc)

  # --- read one band file, cropped to bbox_him ---
  # returns the raster and the mean scan time (seconds after file time) over the cropped rows
  read_band <- function(path) {
    nc <- tidync::tidync(path)

    # band variable, e.g. channel_0007_brightness_temperature (picked by name, not position)
    var_name <- grep("^channel_", nc$variable$name, value = TRUE)
    if (length(var_name) != 1) stop("Could not identify the band variable in ", basename(path))

    b.local <- nc %>%
      tidync::activate(var_name) %>%
      tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
                           x = x > bbox_him[1] & x < bbox_him[3]) %>%
      tidync::hyper_tibble() %>%
      dplyr::mutate(x = as.numeric(x), y = as.numeric(y)) %>%
      dplyr::select(x, y, dplyr::all_of(var_name))
    r <- terra::rast(b.local, type = "xyz", crs = geos_crs)

    # estimated scan time of the cropped rows (seconds after the file time)
    scan_secs <- NA_real_
    if ("scan_line_time" %in% nc$variable$name) {
      scan <- nc %>%
        tidync::activate("scan_line_time") %>%
        tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4]) %>%
        tidync::hyper_tibble()
      scan_secs <- mean(scan$scan_line_time, na.rm = TRUE)
    }

    list(r = r, scan_secs = scan_secs)
  }

  # --- process one time-step; returns a one-row data frame ---
  process_timestep <- function(time_i) {

    dat.i <- df_download %>% dplyr::filter(datetime_utc == time_i)

    # Multi-band image in daytime when every requested band is available, otherwise first band only
    multi <- length(bands) > 1 && dat.i$daynight[1] == "day" && all(bands %in% dat.i$band)
    if (!multi) dat.i <- dat.i %>% dplyr::filter(band == bands[1])
    if (nrow(dat.i) == 0) stop("Band ", bands[1], " not available at this time")

    layers <- lapply(dat.i$path_download, read_band)
    rasts <- purrr::map(layers, "r")

    # Put all bands on the finest grid. Nearest neighbour keeps the original pixel edges
    # of coarser bands (e.g. 2 km B07 on a 1 km grid)
    finest <- which.min(purrr::map_dbl(rasts, ~ terra::res(.x)[1]))
    rasts <- purrr::map(rasts, ~ terra::resample(.x, rasts[[finest]], method = "near"))
    r <- terra::rast(rasts)

    if (stretch) {
      r <- terra::stretch(r, minq = 0.02, maxq = 0.98)
    }

    # Estimated observation time over the fire area (file time + scan offset)
    scan_secs <- mean(purrr::map_dbl(layers, "scan_secs"), na.rm = TRUE)
    obs_time_utc <- if (is.nan(scan_secs)) time_i else time_i + round(scan_secs)
    obs_time_local <- lubridate::with_tz(obs_time_utc, tz = local_tz)

    bands_used <- paste0(dat.i$band, collapse = "")
    out_file <- file.path(dest_folder, paste0(
      dat.i$datetimelocal_chr[1], "_", dat.i$satellite[1], "_", bands_used,
      "_obs", format(obs_time_local, "%Y%m%d%H%M%S"), ".tif"
    ))
    # Stretched values are 0-255. INT2U (not INT1U) because INT1U uses 255 as its NA flag,
    # which would turn the brightest pixels (e.g. fire in B07) into NA
    terra::writeRaster(r, out_file, overwrite = TRUE,
                       datatype = if (stretch) "INT2U" else "FLT4S")

    data.frame(obs_time_utc = obs_time_utc, obs_time_local = obs_time_local,
               bands = bands_used, file = out_file, status = "ok")
  }

  # Loop through time-steps, retrying each up to max_tries times
  results <- purrr::map(seq_along(times_unique), function(i) {
    time_i <- times_unique[i]
    label <- format(lubridate::with_tz(time_i, tz = local_tz), "%Y-%m-%d %H:%M %Z")
    for (attempt in seq_len(max_tries)) {
      out <- tryCatch(process_timestep(time_i), error = function(e) e)
      if (!inherits(out, "error")) {
        message("Saved ", basename(out$file))
        return(cbind(data.frame(datetime_utc = time_i), out))
      }
      message("Attempt ", attempt, "/", max_tries, " failed for ", label, ": ", conditionMessage(out))
      if (attempt < max_tries) Sys.sleep(wait_seconds)
    }
    data.frame(datetime_utc = time_i, obs_time_utc = as.POSIXct(NA), obs_time_local = as.POSIXct(NA),
               bands = NA_character_, file = NA_character_, status = conditionMessage(out))
  })

  results <- dplyr::bind_rows(results)
  n_ok <- sum(results$status == "ok")
  message(n_ok, " of ", nrow(results), " time-steps saved to ", dest_folder)

  invisible(results)
}
