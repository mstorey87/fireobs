#' Sample LFMC or flammability from ANU THREDDS
#'
#' Downloads and samples live fuel moisture content (LFMC) or flammability data
#' from the ANU THREDDS server for a given time and spatial feature.
#'
#' @param datetimeutc POSIXct datetime in UTC timezone.
#' @param sf_data An `sf` object (point or polygon).
#' @param varname Variable to sample: `"fmc"` (live fuel moisture) or `"flam"` (flammability).
#' @param allcells Logical. If `TRUE`, return a data frame of all raster cells intersecting `sf_data`. If `FALSE`, return a single summarised value.
#' @param extract_fun Function to summarise raster values (`"mean"` by default).
#'
#' @return An `sf` object input row id, extracted values, percent of cells that were NA.
#' @export
#'
#' @examples
#' library(sf)
#' # Create a line near Canberra, Australia for 2019
#' my_line <- st_sfc(
#'   st_linestring(rbind(c(149.1, -35.3), c(149.2, -35.25))),
#'   crs = 4326
#' )
#' my_sf <- st_sf(id = 1, geometry = my_line)
#'
#' # Sample FMC for 1 Jan 2019
#' result <- fire_anu_sample(
#'   datetimeutc = as.POSIXct("2019-01-01 00:00:00", tz = "UTC"),
#'   sf_data = my_sf,
#'   varname = "fmc",
#'   allcells = FALSE
#' )
#' print(result)
fire_anu_sample <- function(datetimeutc, sf_data, varname, allcells = FALSE, extract_fun = "mean") {
  # Validate input
  checkmate::assert_posixct(datetimeutc, len = 1)

  # Ensure datetime is UTC
  start_time <- lubridate::with_tz(datetimeutc, tz = "UTC")

  # Get bounding box in WGS84
  bbox <- sf::st_bbox(sf::st_transform(sf_data, 4326))

  # Parse year and date
  myyear <- lubridate::year(start_time)
  mydate <- as.Date(start_time)

  # Match variable name
  varname2 <- switch(
    varname,
    "fmc" = "lfmc_median",
    "flam" = "flammability",
    stop("Invalid varname: must be 'fmc' or 'flam'")
  )

  # Build NetCDF path
  ncpath <- paste0(
    "https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",
    varname, "_c6_", myyear, ".nc"
  )

  # Open NetCDF
  nc_conn <- tidync::tidync(ncpath)

  # Find most recent date before requested date
  nc_dates <- as.Date(nc_conn$transforms$time$timestamp)
  valid_dates <- nc_dates[nc_dates < mydate]

  if (length(valid_dates) == 0) {
    # Try previous year if no valid date found
    ncpath <- paste0(
      "https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",
      varname, "_c6_", myyear - 1, ".nc"
    )
    nc_conn <- tidync::tidync(ncpath)
    nc_dates <- as.Date(nc_conn$transforms$time$timestamp)
    valid_dates <- nc_dates[nc_dates < mydate]
  }

  if (length(valid_dates) == 0) stop("No suitable date found in NetCDF files.")

  closest_date <- tail(valid_dates, 1)
  datediff <- as.numeric(difftime(mydate, closest_date, units = "days"))

  message("Note: extracted date is ", datediff, " days before requested date.")


  # Match NetCDF time index for this date
  datetimeutc_nc <- nc_conn$transforms$time %>%
    dplyr::filter(timestamp == as.character(closest_date)) %>%
    dplyr::pull(time)

  # Extract relevant raster data
  raster_df <- nc_conn %>%
    tidync::activate(varname2) %>%
    tidync::hyper_filter(
      time = time == datetimeutc_nc,
      latitude = latitude >= bbox["ymin"] - 1 & latitude <= bbox["ymax"] + 1,
      longitude = longitude >= bbox["xmin"] - 1 & longitude <= bbox["xmax"] + 1
    ) %>%
    tidync::hyper_tibble() %>%
    dplyr::select(longitude, latitude, dplyr::all_of(varname2))

  # Convert to raster
  r <- terra::rast(raster_df)
  terra::crs(r) <- "EPSG:4326"

  # Extract values
  vals <- terra::extract(r, sf_data, raw = FALSE, ID = FALSE)

  if (isTRUE(allcells)) {
    sf_data[[varname]] <- list(vals)
    result <- sf_data %>%
      dplyr::mutate(extract_date=closest_date)
  } else {

    #calculate how many cells that cross line are NA
    prc_NA <- sum(is.na(vals[,1]))/length(vals[,1])*100

    #extract value summarised
    vals <- terra::extract(r, sf_data, fun = extract_fun, ID = FALSE,na.rm=T)
    names(vals)=paste0(names(vals),"_",extract_fun)


    result <- cbind(sf_data, vals,prc_NA) %>%
      dplyr::mutate(extract_date=closest_date)


  }

  return(result)
}
