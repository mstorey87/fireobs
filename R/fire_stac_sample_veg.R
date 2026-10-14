#' Search Geoscience Australia STAC browser for vegetation data
#'
#' @description
#' Searches the Geoscience Australia STAC browser for vegetation-related data that intersect a given polygon
#' and fall within a specified date range. A maximum of 999 records will be returned,
#' so the location and/or date range may need to be narrowed for large queries.
#'
#' This function uses the Digital Earth Australia STAC API:
#' https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
#' and product information:
#' https://knowledge.dea.ga.gov.au/notebooks/Beginners_guide/03_Products_and_measurements/
#'
#' @param sf_object An `sf` object defining the sampling area.
#' @param start_time The start time of the search window (`POSIXct`). Will be converted to UTC.
#' @param end_time The end time of the search window (`POSIXct`). Will be converted to UTC.
#' @param collection_name The name of the product to search for.
#'   Allowed values include:
#'   - `"ga_ls_fc_pc_cyear_3"` (Fractional Cover Percent Cover Annual)
#'   - `"ga_ls_landcover_class_cyear_3"` (Land Cover Classification Annual)
#'   - `"ga_srtm_dem1sv1_0"` (DEM)
#'   - `"ga_ls_fc_3"` (Fractional Cover Daily)
#'   @param return_rasts Logical. Return a list of terra spatrasts (vsi connections) instead of sampled dataframe
#'
#' @return A data frame containing sampled raster values joined to the input geometry, with variable names
#'   indicating the product and band. If no valid data is found, returns `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Example sf polygon near Canberra
#' my_sf <- sf::st_sfc(
#'   sf::st_linestring(
#'     matrix(
#'       c(149.5, -35.4,
#'         149.6, -35.4),
#'       ncol = 2, byrow = TRUE
#'     )
#'   )
#'
#' ) %>%
#'   sf::st_as_sf(crs = 4326)
#'
#' # Define time window
#' start_time <- as.POSIXct("2023-01-02", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2023-01-21", tz = "Australia/Sydney")
#'
#' # Run query for Fractional Cover daily product
#' result <- fire_stac_sample_veg(sf_object = my_sf,
#'                                start_time = start_time,
#'                                end_time = end_time,
#'                                collection_name = "ga_ls_fc_3")
#'
#' head(result)
#'
#' #no end time needed for yearly veg
#' #note start date will be converted to utc internally
#' result_yearly <- fire_stac_sample_veg(
#' sf_object = my_sf,
#' start_time = start_time,
#' end_time = NULL,
#' collection_name = "ga_ls_fc_pc_cyear_3"
#' )
#'
#' head(result_yearly)
#' }
fire_stac_sample_veg <- function(sf_object,
                                 id_col_name,
                                 start_time = NULL,
                                 end_time = NULL,
                                 collection_name,
                                 return_rasts=FALSE) {

  # Check input: must be sf
  checkmate::assert(inherits(sf_object, "sf"), "Error: sf input object needed")

  # Buffer the input polygon by 1 km (transformed to EPSG:3577, then back to EPSG:4326)
  sf_bbox <- sf_object %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(3577) %>%
    sf::st_buffer(1000) %>%
    sf::st_transform(4326)

  # Compute time difference in days (used for daily products)
  timediff <- as.numeric(difftime(end_time, start_time, units = "days"))

  # Validate and convert start_time to UTC ISO string
  if (!is.null(start_time)) {
    checkmate::assert(stringr::str_detect(class(start_time)[1], "POSIXct"), "Error: times must be posixct")
    start_time <- lubridate::with_tz(start_time, tz = "UTC")
    start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  }

  # Validate and convert end_time to UTC ISO string
  if (!is.null(end_time)) {
    checkmate::assert(stringr::str_detect(class(end_time)[1], "POSIXct"), "Error: times must be posixct")
    end_time <- lubridate::with_tz(end_time, tz = "UTC")
    end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")
  }

  # Connect to the DEA STAC source
  stac_source <- rstac::stac("https://explorer.dea.ga.gov.au/stac")

  # Define the datetime string used in the query based on product type
  if (collection_name == "ga_srtm_dem1sv1_0") {
    # DEM has no time dimension
    datetime_chr <- NULL
    message("sampling ", collection_name)
  }

  if (collection_name %in% c("ga_ls_landcover_class_cyear_3", "ga_ls_fc_pc_cyear_3")) {
    # For annual products, use 1st Jan of the year
    year <- lubridate::year(start_time)
    my_time1 <- paste0(year, "-01-01T00:00:00Z")
    datetime_chr <- paste0(my_time1, "/", my_time1)
    message(paste0("sampling ", collection_name, " for ", datetime_chr))
  }

  if (collection_name == "ga_ls_fc_3") {
    # For daily Landsat Fractional Cover, ensure range is long enough for coverage
    checkmate::assert(timediff > 14, "Error: For daily data, the date range must be > 14 days.")
    datetime_chr <- paste0(start_time, "/", end_time)
    message(paste0("sampling ", collection_name, " for ", datetime_chr))
  }

  # Run STAC search with bbox, product collection and datetime range
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(sf_bbox, 4326)),
    collections = collection_name,
    datetime = datetime_chr,
    limit = 999
  )

  executed_stac_query <- rstac::get_request(stac_query)
  items_list <- executed_stac_query$features

  #check if anything returned
 if(length(items_list)==0){
   message("Nothing returned. Perhaps try different search window or location")
   return(NA)

 }


  # Parse returned STAC items; only available properties are kept using this method
  items_df2 <- purrr::map_df(items_list, function(item) {
    dplyr::tibble(
      title = item$properties$title,
      product = item$properties$`odc:product`,
      datetime = item$properties$datetime,
      end_datetime = item$properties$end_datetime,
      start_datetime = item$properties$start_datetime,
      epsg = item$properties$`proj:epsg`,
      instrument = paste0(item$properties$instruments, collapse = ";"),
      assets = item$assets
    )
  })

  # Extract hrefs for tiff assets, drop QA bands
  dat.x <- items_df2 %>%
    dplyr::mutate(
      href = purrr::map(assets, ~ .x$href),
      asset_title = purrr::map(assets, ~ .x$title)
    ) %>%
    tidyr::unnest(c(href, asset_title)) %>%
    dplyr::filter(stringr::str_ends(href, ".tif"), asset_title != "qa") %>%
    dplyr::select(-assets)

  # Add variable names for each sample
  if ("product" %in% names(dat.x)) {
    dat.x <- dat.x %>% dplyr::mutate(sample_name = paste0(product, "__", asset_title))
  } else {
    dat.x <- dat.x %>% dplyr::mutate(sample_name = paste0(asset_title))
  }

  # Stop if nothing found
  if (!nrow(dat.x) > 0) {
    message("no stac data found for ", collection_name, " ", datetime_chr)
    return(NA)
  }

  # Build https path for public data and sample rasters
  dat.aws <- dat.x %>%
    dplyr::mutate(
      href = stringr::str_replace(
        href, "s3://dea-public-data",
        "https://dea-public-data.s3.ap-southeast-2.amazonaws.com"
      ),
      r = purrr::map(href, ~ terra::rast(.x, vsi = TRUE)),
      res = purrr::map(r, ~ terra::extract(.x, sf::st_transform(sf_object, sf::st_crs(.x)), ID=F,xy = TRUE, cell = TRUE,bind=F) %>%
                                  dplyr::rename(value=1,cellx=x,celly=y,cellno=cell) %>%
                         cbind(sf_object %>% dplyr::select(dplyr::all_of(id_col_name)) %>% sf::st_drop_geometry()))
    )


  if(return_rasts==T){
    return(dat.aws$r)
  }


  # Clean up extracted values and pivot to wide format
  #may be NA value because of different tiles
  res <- dat.aws %>%
    dplyr::select(sample_name, instrument, dplyr::matches("datetime"), res) %>%
    #dplyr::mutate(res = purrr::map(res, ~ setNames(.x, c("sf_id", "value", "cell_no", "cell_x", "cell_y")))) %>%
    tidyr::unnest(cols = res) %>%
    dplyr::arrange(sample_name, value) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      sample_name = stringr::str_replace(sample_name, "_href", ""),
      cellx = round(cellx, 5),
      celly = round(celly, 5)
    )

  res <- tidyr::pivot_wider(res, values_from = value, names_from = sample_name) %>%
    dplyr::select(dplyr::all_of(id_col_name),dplyr::everything())

  #check if any ids are missing.
  # add in missing ids as empty rows
  sf_object_missing <- sf_object%>%
    dplyr::select(dplyr::all_of(id_col_name)) %>%
    sf::st_drop_geometry() %>%
    dplyr::anti_join(res, by = id_col_name)  # removes rows with matching id


  if(nrow(sf_object_missing)>0){
    res <- dplyr::bind_rows(res,sf_object_missing)
  }


  res <- res %>%
    dplyr::arrange(.data[[id_col_name]],datetime)

  return(res)
}

