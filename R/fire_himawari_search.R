#' Search for Himawari-8/9 satellite data on NCI THREDDS
#'
#' @description
#' This function searches the Bureau of Meteorology (BOM) Himawari 8 and 9 satellite image archives
#' hosted on the NCI THREDDS server: \url{https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f8433_0020_1861_5916}.
#'
#' It finds file paths for imagery covering the specified fire bounding box and time range.
#' Himawari images are available every 10 minutes from 1 July 2015; use a larger timestep to reduce the volume of results.
#' The archive (arc) catalogue lags real time by several weeks. Any timestep not found in the archive
#' is also searched for in the near real-time (NRT) catalogue, so recent fires are covered.
#'
#' The output is a data frame listing the file paths for all available band × time combinations.
#'
#' @param fire_bbox An `sf` polygon that defines the spatial search area (e.g., your fire boundary or bounding box).
#' @param start_time A `POSIXct` date-time (any time zone) marking the beginning of the search window. Converted to UTC internally.
#' @param end_time A `POSIXct` date-time (any time zone) marking the end of the search window. Converted to UTC internally.
#' @param timestep_minutes Timestep in minutes for selecting imagery. Must be a multiple of 10.
#'   E.g., `10` for every image or `60` for hourly images.
#'
#' @return A data frame with one row per available band × time, with UTC and local times, file names,
#'   download URLs, band, satellite, and `source` (`"arc"` for the archive or `"nrt"` for near real-time).
#'   If no data is found, a message is printed and `NULL` is returned (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Example: define a small bounding box somewhere in Australia
#' my_bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = 150, ymin = -35, xmax = 151, ymax = -34), crs = 4326)) |> sf::st_sf()
#'
#' # Define time window for search (hourly)
#' my_start <- as.POSIXct("2023-11-01 00:00:00", tz = "Australia/Sydney")
#' my_end <- as.POSIXct("2023-11-01 12:00:00", tz = "Australia/Sydney")
#'
#' # Run search
#' dat_himawari <- fire_himawari_search(
#'   fire_bbox = my_bbox,
#'   start_time = my_start,
#'   end_time = my_end,
#'   timestep_minutes = 60
#' )
#'
#' head(dat_himawari)
#' }
fire_himawari_search <- function(fire_bbox,
                                 start_time,
                                 end_time,
                                 timestep_minutes = 10) {

  # Check inputs
  checkmate::assert_posixct(start_time, len = 1, any.missing = FALSE)
  checkmate::assert_posixct(end_time, len = 1, any.missing = FALSE)
  checkmate::assert_int(timestep_minutes, lower = 10)
  if (timestep_minutes %% 10 != 0) stop("timestep_minutes must be a multiple of 10")
  # compare as numbers (seconds since 1970 UTC) to avoid warnings when time zones differ
  if (as.numeric(end_time) < as.numeric(start_time)) stop("end_time must be after start_time")
  if (as.numeric(end_time) < as.numeric(as.POSIXct("2015-07-01", tz = "UTC"))) {
    stop("Himawari data on NCI starts 1 July 2015 (UTC); choose a later end_time")
  }

  arc_path <- "https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/v1-0"

  # Widen the window to whole timesteps (max 60 min) so no requested image is missed:
  # start rounds down, end rounds up
  round_unit <- paste0(min(timestep_minutes, 60), " minutes")
  start_utc <- lubridate::floor_date(lubridate::with_tz(start_time, tz = "UTC"), round_unit)
  end_utc <- lubridate::ceiling_date(lubridate::with_tz(end_time, tz = "UTC"), round_unit)

  dates_fires <- seq(start_utc, end_utc, by = paste0(timestep_minutes, " mins"))

  # Local timezone of the fire, for local time columns and day/night
  my_tz <- fire_get_timezone(fire_bbox)
  if (is.na(my_tz)) stop("Could not determine a time zone for fire_bbox")

  # One archive catalogue page per timestep; list the band files on each page
  # (NA if the page does not exist)
  dat.paths <- data.frame(datetime_utc = dates_fires) %>%
    dplyr::mutate(
      path_catalog = paste0(arc_path, format(datetime_utc, format = "/%Y/%m/%d/%H%M"), "/catalog.html"),
      source = "arc",
      file_name = purrr::map(path_catalog, ~ fire_thredds_list(.x, "ABOM_OBS_"))
    )

  # The archive lags real time by several weeks. For timesteps with no archive files,
  # look in the near real-time (NRT) catalogue instead
  arc_missing <- purrr::map_lgl(dat.paths$file_name, ~ all(is.na(.x)))
  if (any(arc_missing)) {
    dat.paths.nrt <- dat.paths[arc_missing, ] %>%
      dplyr::mutate(
        path_catalog = stringr::str_replace(path_catalog, "/arc/obs/", "/nrt/obs/"),
        source = "nrt",
        file_name = purrr::map(path_catalog, ~ fire_thredds_list(.x, "ABOM_OBS_"))
      )
    dat.paths <- dplyr::bind_rows(dat.paths, dat.paths.nrt)
  }

  dat.paths <- dat.paths %>%
    tidyr::unnest(cols = "file_name") %>%
    dplyr::filter(!is.na(file_name))

  if (nrow(dat.paths) == 0) {
    message("No Himawari images found. If the search window is recent, the data may not be on NCI yet.")
    return(invisible(NULL))
  }

  dat.paths <- dat.paths %>%
    dplyr::mutate(
      # Read band and satellite from the file name by pattern, not position
      # (file names differ in length between 500 m, 1 km and 2 km bands)
      band = stringr::str_extract(file_name, "(?<=ABOM_OBS_)B\\d{2}"),
      satellite = stringr::str_extract(file_name, "HIMAWARI\\d"),
      path_download = paste0(stringr::str_replace(dirname(path_catalog), "/thredds/catalog/", "/thredds/dodsC/"),
                             "/", file_name)
    ) %>%
    # If an image is in both the archive and NRT catalogues, keep the archive copy
    dplyr::arrange(datetime_utc, band, source) %>%
    dplyr::distinct(datetime_utc, band, .keep_all = TRUE) %>%
    dplyr::mutate(
      datetimelocal = lubridate::with_tz(datetime_utc, tz = my_tz),
      datetimelocal_chr = format(datetimelocal, format = "%Y%m%d%H%M%S"),
      date_local = format(datetimelocal, format = "%Y-%m-%d"),
      hour_local = lubridate::hour(datetimelocal),
      minutes_local = lubridate::minute(datetimelocal),
      daynight = ifelse(hour_local >= 8 & hour_local <= 18, "day", "night")
    ) %>%
    dplyr::select(
      datetime_utc, datetimelocal, datetimelocal_chr, date_local,
      hour_local, minutes_local, daynight, path_catalog,
      file_name, path_download, band, satellite, source
    )

  return(dat.paths)
}
