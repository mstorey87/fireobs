#' Search NASA FIRMS hotspots API
#'
#' This function queries the NASA FIRMS (Fire Information for Resource Management System)
#' API for thermal hotspot detections (e.g., VIIRS, MODIS) within a specified bounding box
#' and date range. It automatically handles the API's 5-day request limit by splitting
#' queries into chunks, downloads the hotspot CSV files, merges them, transforms the
#' coordinates to GDA94, adds local time columns, and saves a GeoPackage.
#'
#' @param fire_bbox An `sf` polygon defining the bounding box for the search area.
#' @param mapkey Your FIRMS API map key, which you can request at:
#'   https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' @param start_date Start date of the search period in `YYYY-mm-dd` format.
#' @param end_date End date of the search period in `YYYY-mm-dd` format.
#' @param dest_folder Local folder path to temporarily download CSV files
#'   and to save the final GeoPackage.
#' @param product_filter Optional. A regex string to filter products
#'   (e.g., `'VIIRS'`, `'MODIS'`, `'SNPP'`, `'NOAA20'`, `'NOAA21'`). See fire_GIBS_products() for list of available products.
#'
#' @return An `sf` POINT object of detected hotspots, with UTC and local times,
#'   satellite product info, and written GeoPackage.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Example bounding box around Canberra, Australia
#' bbox <- st_as_sfc(st_bbox(c(xmin=148.8, ymin=-35.5, xmax=149.4, ymax=-35.1), crs=4326))
#'
#' # Replace with your FIRMS API map key and an existing folder path
#' # get a mapkey here: https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' my_mapkey <- "YOUR_FIRMS_MAPKEY"
#'
#' # Search for hotspots in January 2024, filtering for VIIRS only
#' hotspots <- fire_hotspots_search(
#'   fire_bbox = bbox,
#'   mapkey = my_mapkey,
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-31",
#'   dest_folder = "C:/temp_data",
#'   product_filter = "VIIRS"
#' )
#'
#' print(hotspots)
#' }
fire_hotspots_search <- function(fire_bbox,mapkey,start_date,end_date,dest_folder,product_filter=NULL){

  # Validate that start and end times are POSIXct
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (anyNA(c(start_date, end_date))) stop("start_date and end_date must be dates, e.g. '2024-01-31'")
  if (end_date < start_date) stop("end_date must be on or after start_date")

  checkmate::assert(!is.null(mapkey),"NASA FIRMS API mapkey needed to download hotspot data. Get yours at https://firms.modaps.eosdis.nasa.gov/api/map_key/")
  checkmate::assert(file.exists(dest_folder),"Error: dest_folder does not exist")

  # Increase timeout for large downloads
  options(timeout = 240)

  # Reproject fire bbox to EPSG:4326, required for WMS and hotspot queries
  fire_bbox <- fire_bbox %>% sf::st_transform(4326)

  # NASA FIRMS API allows max 5 days per request (it returns "Invalid day range. Expects [1..5]").
  # Create chunks of up to 5 days.
  max_days_per_request <- 5
  n_days <- as.numeric(difftime(end_date,start_date,units="days")) + 1
  date_seq <- as.character(seq(as.Date(start_date), as.Date(end_date), by = "1 day"))
  date_groups <- split(date_seq, ceiling(seq_along(date_seq) / max_days_per_request))
  date_groups <- paste0(unlist(lapply(date_groups, length)), "/", unlist(lapply(date_groups, `[[`, 1)))

  # Create bbox coordinates string
  box_coords <- paste0(sf::st_bbox(fire_bbox), collapse = ",")

  # Available products
  products <- c("VIIRS_SNPP_NRT", "VIIRS_SNPP_SP", "VIIRS_NOAA20_NRT", "VIIRS_NOAA20_SP",
                "VIIRS_NOAA21_NRT", "MODIS_SP", "MODIS_NRT")
  if(!is.null(product_filter)){
    products <- products[stringr::str_detect(products, product_filter)]
  }

  # Build all combinations of products x date groups
  path_api <- paste0("https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
                     mapkey, "/", products, "/", box_coords, "/")

  path_api <- expand.grid(path_api, date_groups) %>%
    dplyr::mutate(pth = paste0(Var1, Var2)) %>%
    .$pth

  # Local file paths for download
  out.files <- file.path(dest_folder, paste0("hs_", seq_along(path_api), "_",
                                             purrr::map_chr(stringr::str_split(path_api, "/"), 8), ".csv"))

  # Download each chunk
  for(xi in seq_along(path_api)){
    utils::download.file(path_api[xi], out.files[xi], mode = "wb")
  }

  # Load and combine all hotspot CSVs
  dat.hotspots <- purrr::map(
    out.files,
    ~readr::read_csv(.x, show_col_types = FALSE) %>%
      dplyr::mutate(across(everything(), as.character))
  ) %>%
    dplyr::bind_rows() %>%
    readr::type_convert()

  # Remove downloaded CSVs
  file.remove(out.files)

  # If data is present, convert to sf and add details
  if(nrow(dat.hotspots) > 0){
    dat.hotspots <- sf::st_as_sf(dat.hotspots, coords = c("longitude", "latitude"), crs = 4326) %>%
      sf::st_transform(4283)

    # Standardize satellite name
    dat.hotspots <- dat.hotspots %>%
      dplyr::mutate(
        sat_name = dplyr::case_when(
          satellite == "N" ~ "SNPP",
          satellite == "N20" ~ "NOAA20",
          satellite == "N21" ~ "NOAA21",
          TRUE ~ satellite
        ),
        sat_name = tolower(paste0(instrument, "_", sat_name))
      )

    # Look up timezone for bbox centroid
    my_tz <- fire_get_timezone(fire_bbox)

    # Format UTC datetime and add local time
    dat.hotspots <- dat.hotspots %>%
      dplyr::mutate(
        acq_time = paste0("0000", acq_time),
        acq_time = substr(acq_time, nchar(acq_time) - 3, nchar(acq_time)),
        datetimeutc = lubridate::fast_strptime(
          paste0(acq_date, " ", acq_time),
          format = "%Y-%m-%d %H%M",
          tz = "UTC"
        ),
        timezone = my_tz,
        datetimelocal = lubridate::with_tz(datetimeutc, tzone = timezone),
        datelocal = format(datetimelocal, format = "%Y-%m-%d")
      )

    # Write to GeoPackage with abbreviated column names
    sf::st_write(
      dat.hotspots %>%
        dplyr::mutate(
          datetimeutc = as.character(datetimeutc),
          datetimelocal = as.character(datetimelocal)
        ) %>%
        dplyr::rename_with(~abbreviate(., minlength = 10)),
      paste0(dest_folder, "\\hotspots.gpkg"),
      delete_dsn = TRUE
    )
  }

  options(timeout = 60)
  return(dat.hotspots)
}




