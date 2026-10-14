#' Construct HTTPS URL to a BARRA data file on NCI THREDDS server
#'
#' Generates the full URL string pointing to a specific BARRA NetCDF file
#' on the NCI THREDDS server based on datetime, resolution, timestep, and variable.
#'
#' @param datetimeutc POSIXct datetime in UTC timezone, ideally rounded to the nearest hour.
#' @param barraid Character; either `"R2"` (12 km BARRA product) or `"C2"` (~4 km BARRA product).
#' @param timestep Character; time resolution of data, one of `"hourly"`, `"daily"`, or `"monthly"`.
#' @param varname Character; BARRA variable name (e.g., `"sfcWind"` for surface wind, `"tas"` for temperature,
#'   `"hurs"` for relative humidity, `"uas"` and `"vas"` for wind components).
#'   See [BOM report](http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf) for details.
#'
#' @return Character string giving the full HTTPS URL to the requested BARRA NetCDF file.
#' @export
#'
#' @examples
#' \dontrun{
#' fire_barra_path(
#'   datetimeutc = as.POSIXct("2019-12-01 10:00:00", tz = "UTC"),
#'   barraid = "R2",
#'   timestep = "hourly",
#'   varname = "sfcWind"
#' )
#' }
fire_barra_path <- function(datetimeutc, barraid, timestep, varname) {
  # Validate inputs
  checkmate::assert_choice(barraid, c("R2", "C2"), .var.name = "barraid")
  checkmate::assert_choice(timestep, c("hourly", "daily", "monthly"), .var.name = "timestep")
  checkmate::assert_string(varname)
  ## ensure utc
  checkmate::assert_posixct(datetimeutc, any.missing = FALSE)
  if (toupper(lubridate::tz(datetimeutc)) != "UTC") stop("datetimeutc must be in UTC")

  # Map barraid to internal product code used in paths
  barraid1 <- switch(barraid,
                     R2 = "AUST-11",
                     C2 = "AUST-04")

  # Format year and month string (e.g., "201912")
  yrmnth <- format(datetimeutc, "%Y%m")

  # Base URL path components by timestep
  timestep_folder <- switch(timestep,
                            hourly = "1hr",
                            daily = "day",
                            monthly = "mon")

  # Construct filename matching NCI naming convention
  file_thredds <- paste0(
    varname, "_",
    barraid1, "_ERA5_historical_hres_BOM_BARRA-",
    barraid, "_v1_", timestep_folder, "_",
    yrmnth, "-", yrmnth, ".nc"
  )

  # Construct base NCI THREDDS URL
  nci_path <- paste0(
    "https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",
    barraid1, "/BOM/ERA5/historical/hres/BARRA-",
    barraid, "/v1/", timestep_folder, "/", varname, "/latest"
  )

  # Full URL to the file
  full_url <- paste0(nci_path, "/", file_thredds)

  return(full_url)
}
