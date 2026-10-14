#' Sample monthly precipitation from AGCD (Australian Gridded Climate Data)
#'
#' This function samples monthly total precipitation values from the AGCD
#' gridded dataset for a set of spatial features with associated timestamps.
#' It matches each feature's date to the appropriate NetCDF file on the
#' NCI THREDDS server, extracts the relevant grid cells, and computes
#' basic precipitation summaries within a buffer around each feature.
#'
#' @param dat An `sf` object with geometry column and a POSIXct time column in UTC.
#' @param time_utc_col Character name of the column in `dat` containing the time.
#' @param buffer_m Numeric. Buffer distance in meters around the geometry to sample grid points.
#'
#' @returns A data frame with mean, min, max, and median precipitation statistics
#' for each feature and the date sampled.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Sample AGCD monthly precipitation for points
#' library(sf)
#' library(lubridate)
#'
#' # Create two points with dates in January and February 2020
#' pts <- st_sf(
#'   geometry = st_sfc(
#'     st_point(c(150, -35)),
#'     st_point(c(151, -36))
#'   ),
#'   datetime_utc = as.POSIXct(
#'     c("2020-01-15 00:00:00", "2020-02-15 00:00:00"),
#'     tz = "UTC"
#'   ),
#'   crs = 4326
#' )
#'
#' # Run the function with a 10,000 m buffer
#' result <- fire_sample_agcd(pts, time_utc_col = "datetime_utc", buffer_m = 10000)
#' head(result)
#' }
fire_sample_agcd <- function(dat,time_utc_col,buffer_m){

  dat[["tm"]] <- dat[[time_utc_col]]

  # Ensure time column is POSIXct UTC
  checkmate::assert(unique(stringr::str_detect(class(dat$tm),"POSIX")),
                    "Error: Time column must be POSIXct")
  checkmate::assert(lubridate::tz(dat$tm)=="UTC",
                    "Error: Time column must have timezone UTC")

  # Add year, month, file path, and row ID
  dat.x <- dat %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      yr = lubridate::year(tm),
      mnth = lubridate::month(tm),
      thredds_path = paste0(
        "https://thredds.nci.org.au/thredds/dodsC/zv2/agcd/v2-0-3/precip/total/r001/01month/agcd_v2_precip_total_r001_monthly_",
        yr, ".nc"
      ),
      input_rowid = dplyr::row_number()
    )

  # Unique NetCDF paths to loop over
  unq_paths <- unique(dat.x$thredds_path)
  dat.sampled.all <- list()

  for(i in seq_along(unq_paths)){

    dat.i <- dat.x %>% dplyr::filter(thredds_path == unq_paths[i])
    mnths.i <- unique(dat.i$mnth)

    # Connect to NetCDF
    nc_conn <- tidync::tidync(unq_paths[i])

    # Get NetCDF timestamps for required month(s)
    datetimeutc_nc.all <- nc_conn$transforms$time %>%
      dplyr::filter(index == mnths.i) %>%
      data.frame()

    dat.sampled <- list()
    for(xi in 1:nrow(datetimeutc_nc.all)){

      datetimeutc_nc <- datetimeutc_nc.all[xi,]
      dat.xi <- dat.i %>%
        dplyr::filter(mnth == datetimeutc_nc$index)

      checkmate::assert(
        lubridate::month(datetimeutc_nc$timestamp) == unique(dat.xi$mnth),
        "Error: NC month doesn't match sample data month"
      )

      bbox <- sf::st_bbox(dat.xi)

      # Filter NetCDF for this time & bbox
      nc_filt <- nc_conn %>%
        tidync::activate("precip") %>%
        tidync::hyper_filter(
          time = time == datetimeutc_nc$time,
          lat = lat >= bbox[2] - 1 & lat <= bbox[4] + 1,
          lon = lon >= bbox[1] - 1 & lon <= bbox[3] + 1
        ) %>%
        tidync::hyper_tibble() %>%
        dplyr::select(lon, lat, precip)

      nc_filt$lon <- as.numeric(nc_filt$lon)
      nc_filt$lat <- as.numeric(nc_filt$lat)

      # Convert to sf points and reproject
      x <- sf::st_as_sf(nc_filt, coords = c("lon", "lat"), crs = 4326) %>%
        sf::st_transform(3112)

      # Buffer input geometries
      dat.xi.buff <- dat.xi %>%
        sf::st_transform(3112) %>%
        sf::st_buffer(buffer_m) %>%
        dplyr::select(input_rowid)

      # Intersect points and buffers
      suppressWarnings({
        s <- sf::st_intersection(x, dat.xi.buff)
      })

      # Summarise precipitation stats for each input feature
      z <- s %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(input_rowid) %>%
        dplyr::summarise(
          agcd_precip_mean = mean(precip),
          agcd_precip_min = min(precip),
          agcd_precip_max = max(precip),
          agcd_precip_median = median(precip),
          agcd_date = datetimeutc_nc$timestamp
        )

      dat.sampled[[xi]] <- z
    }

    dat.sampled.all[[i]] <- do.call(rbind, dat.sampled)
  }

  dat.sampled.all.2 <- do.call(rbind, dat.sampled.all) %>%
    dplyr::arrange(input_rowid)

  return(dat.sampled.all.2)
}


