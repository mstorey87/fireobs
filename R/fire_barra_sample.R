#' #' Function to stream and sample BARRA weather netcdf
#' #'
#' #' @description
#' #' Take an existing nc connection (tidync::tidync()) and sample for the locations in the input sf points
#' #' see end of this document for variable names http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#' #' example here of R2 variables on thredds https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/catalog.html
#' #' some variables are sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components). These are the values on the hour. Some variables
#' #' @param nc_conn netcdf connection using tidync::tidync()
#' #' @param datetimeutc posixct datetime in utc. Must match https path of nc connection
#' #' @param sf_data sf object with one row with locations to sample barra data
#' Function to stream and sample BARRA weather netcdf
#'
#' @description
#' Takes an existing NetCDF connection (created via `tidync::tidync()`) and samples
#' the specified BARRA variable for the locations in the input `sf` object.
#'
#' Variable names and details can be found here:
#' http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#'
#' Example of R2 variables on THREDDS:
#' https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/catalog.html
#'
#' Common variables include:
#' - `sfcWind` (surface wind)
#' - `tas` (temperature)
#' - `hurs` (relative humidity)
#' - `vas` and `uas` (wind components)
#'
#' @param nc_conn NetCDF connection created with `tidync::tidync()`
#' @param datetimeutc POSIXct datetime in UTC; must match time dimension in NetCDF connection
#' @param sf_data `sf` object (typically points or polygons) with locations to sample
#' @param varname Character; BARRA variable name matching variable in NetCDF (e.g. "sfcWind", "tas")
#' @param return_rast Logical; if TRUE, a terra spatrast is returned. If FALSE, a data frame of sampled
#'                    values are returned.
#' @param allcells Logical; if TRUE returns all intersecting cells in BARRA data for polygons;
#'                 if FALSE (default), returns summarized value (e.g. mean) for points or polygons
#' @param timestep Character; one of "hourly", "daily", or "monthly". Determines time filtering.
#' @param extract_fun Character or function; summary function used by `terra::extract()` when `allcells=FALSE`
#'
#' @return An `sf` object with new columns containing sampled variable data
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidync)
#' library(sf)
#'
#' # Create NetCDF connection to a BARRA R2 variable (e.g., sfcWind)
#' nc <- tidync("https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/sfcWind/latest/sfcWind_AUS-11_ERA5_historical_hres_BOM_BARRA-R2_v1_1hr_201912-201912.nc")
#'
#' # Create example sf points near Canberra
#' pts <- sf::st_sf(
#'   id = 1,
#'   geometry = sf::st_sfc(sf::st_point(c(149.1, -35.3)), crs = 4326)
#' )
#'
#' # Sample data for given datetime and points
#' res <- fire_barra_sample(
#'   nc_conn = nc,
#'   datetimeutc = as.POSIXct("2019-12-01 10:00:00", tz = "UTC"),
#'   sf_data = pts,
#'   varname = "sfcWind",
#'   allcells = FALSE,
#'   timestep = "hourly",
#'   extract_fun = "mean"
#' )
#' }
fire_barra_sample <- function(nc_conn, datetimeutc, sf_data, varname,
                              return_rast=FALSE,
                              allcells = FALSE, timestep, extract_fun = "mean") {
  # see end of this document for variable names http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
  # example here of R2 variables on thredds https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/catalog.html
  # some variables are sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components). These are the values on the hour. Some variables
  # have variations e.g. tasmean is mean hourly temperature

  ## ensure utc
  checkmate::assert_posixct(datetimeutc,len=1)
  if (toupper(lubridate::tz(datetimeutc)) != "UTC") stop("datetimeutc must be in UTC")


  # Transform input sf object to lat-lon (EPSG:4326) for spatial filtering
  sf_data <- sf_data %>% sf::st_transform(4326)

  # Get bounding box of sf_data for spatial filtering in NetCDF
  bbox <- sf::st_bbox(sf_data)

  # Validate timestep input
  checkmate::assert(timestep %in% c("hourly", "daily", "monthly"), "Error: timestep must be hourly, daily or monthly")

  # Extract time index from NetCDF connection based on timestep and datetime
  if (timestep == "hourly") {
    datetimeutc_nc <- nc_conn$transforms$time

    #some of the max variable give the half hourly time
    #check to see.

    mins <- datetimeutc_nc$timestamp %>% substr(15,16) %>% unique()

    if(mins=="30"){
      #if barra times are given as on the half hour, we want to minus 30 from input datetime, so the extracted values is the max for the hour prior to out datetime
      mnth <- lubridate::month(datetimeutc)
      datetimeutc_adjusted <- datetimeutc-lubridate::minutes(30)
      #check if adjusting time has pushed the datetime back a month, if so we need to load the prior nc file
      mnthcheck <- lubridate::month(datetimeutc_adjusted)==mnth
      if(!mnthcheck){
        ncsource <- nc_conn$source$source
        nc_mnth <- substr(ncsource,nchar(ncsource)-8,nchar(ncsource)-3)
        nc_date <- as.Date(paste0(nc_mnth,"01"),format="%Y%m%d")
        new_nc_date <- nc_date-lubridate::weeks(1)
        new_nc_mnth <- format(new_nc_date,"%Y%m-%Y%m.nc")
        new_ncsource <- paste0(substr(ncsource,1,nchar(ncsource)-16),new_nc_mnth)
        nc_conn <- fire_tidync_safe(new_ncsource)
        datetimeutc_nc <- nc_conn$transforms$time

      }



      datetimeutc_nc <- datetimeutc_nc %>%
        dplyr::filter(timestamp == format(datetimeutc_adjusted, "%Y-%m-%d %H:%M:%S") |
                        timestamp == format(datetimeutc_adjusted, "%Y-%m-%dT%H:%M:%S")) %>% .$time

    }


    if(mins=="00"){
      datetimeutc_nc <- datetimeutc_nc %>%
        dplyr::filter(timestamp == format(datetimeutc, "%Y-%m-%d %H:%M:%S") |
                        timestamp == format(datetimeutc, "%Y-%m-%dT%H:%M:%S")) %>% .$time

    }


  }

  if (timestep == "daily") {
    checkmate::assert(unique(lubridate::hour(datetimeutc)) == 12,
                      "Error: to sample daily data, 12 PM times UTC are needed")
    datetimeutc_nc <- nc_conn$transforms$time %>%
      dplyr::filter(timestamp == format(datetimeutc, "%Y-%m-%d %H:%M:%S") |
                      timestamp == format(datetimeutc, "%Y-%m-%dT%H:%M:%S")) %>% .$time
  }

  if (timestep == "monthly") {
    datetimeutc_nc <- nc_conn$transforms$time$time[1]
  }

  # Check only one time slice is returned
  checkmate::assert(length(datetimeutc_nc) == 1, "Error with nc time filtering")

  # Activate NetCDF variable and apply spatial and temporal filters
  #add a 1 degree buffer to extract
  b.local <- nc_conn %>%
    tidync::activate(varname) %>%
    tidync::hyper_filter(
      time = time == datetimeutc_nc,
      lat = lat >= bbox[2] - 1 & lat <= bbox[4] + 1,
      lon = lon >= bbox[1] - 1 & lon <= bbox[3] + 1
    ) %>%
    tidync::hyper_tibble() %>%
    #use matches and everything to ensure var is 3rd column, additional column (e.g. depth) after
    dplyr::select(lon, lat,dplyr::matches(varname),dplyr::everything(),-dplyr::matches("time"))

  #check if there is a 4th layer column, e.g. depth. if so pivot wider so each col becomes a layer
  b.local_cols <- names(b.local)

  if(length(b.local_cols)>=4){
    depth.cols <- b.local_cols[4:length(b.local_cols)]
    b.local <-  tidyr::pivot_wider(b.local,
                                  values_from = dplyr::all_of(varname),
                                  names_from = dplyr::all_of(depth.cols),
                                               names_prefix = paste0(varname, "_"))


  }

  # Create raster from filtered data
  r <- terra::rast(b.local)
  terra::crs(r) <- "epsg:4326"  # WGS84 geographic coordinate system
  #terra::writeRaster(r,paste0("D:\\temp\\rast1",format(datetimeutc, "%Y%m%dT%H%M%S"),".tif"),overwrite=T)

  #if user just wants a rast returned:
  if(return_rast==TRUE){

    return(r)
  }




  # Extract raster values for input sf locations
  if (allcells == FALSE & return_rast==FALSE) {
    res <- terra::extract(r, sf_data, fun = extract_fun, ID = FALSE, na.rm=TRUE)
    names(res) <- paste0(names(res), "_", extract_fun)
    res <- cbind(sf_data, res)
    return(res)
  }

  # If allcells = TRUE, return all intersecting raster cells as a list-column
  if (allcells == TRUE & return_rast==FALSE) {
    res <- terra::extract(r, sf_data, raw = FALSE, ID = FALSE)
    sf_data[[varname]] <- list(res)
    res <- sf_data
    return(res)
  }

  return(res)
}


